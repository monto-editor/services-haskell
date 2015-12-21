{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (length)

import           AST
import           ErrorMessages
import           SourceSpan
import           Tokens
import           Outline

import           GHC hiding (Type,language)
import           GHC.Paths ( libdir )

import           Digraph hiding (path)
import           DynFlags hiding (language)
import           Bag
import           HscTypes
import           GhcMonad
import           SysTools

import           Control.Exception
import           Control.Monad

import           Data.Aeson

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Tree
import qualified Data.Vector as V

import           System.ZMQ4 (Context)
import qualified System.ZMQ4 as Z
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import           System.Exit
import           System.Directory

import           Text.Printf

import           Monto.ProductMessage (ProductMessage(ProductMessage))
import qualified Monto.ProductMessage as P
import           Monto.DeregisterService (DeregisterService(DeregisterService))
import           Monto.RegisterServiceRequest (RegisterServiceRequest(RegisterServiceRequest))
import qualified Monto.RegisterServiceRequest as R
import           Monto.RegisterServiceResponse
import           Monto.ServiceDependency
import           Monto.Types hiding (fromText)
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V

data ZMQConfig = ZMQConfig
               { zeromqContext :: Context
               , serviceAddress :: String
               , registrationAddress :: String
               , configurationAddress :: String
               }

data Interrupted = Interrupted
  deriving (Eq,Show)

main :: IO ()
main =
  Z.withContext $ \ctx -> do
    _ <- installHandler sigINT  (Catch exitSuccess) Nothing
    _ <- installHandler sigTERM (Catch exitSuccess) Nothing
    outlineIcons <- getOutlineIcons

    let cfg = ZMQConfig
            { zeromqContext = ctx
            , serviceAddress = "tcp://localhost:"
            , registrationAddress = "tcp://localhost:5004"
            , configurationAddress = "tcp://localhost:5007"
            }

    eitherErrorOrResp <- register cfg RegisterServiceRequest
            { R.serviceID = "ghc"
            , R.label = "Glasgow Haskell Compiler"
            , R.description = "The GHC compiler does tokenization, parsing and typechecking"
            , R.options = Nothing
            , R.dependencies = V.fromList [ SourceDependency "haskell" ]
            , R.language = "haskell"
            , R.products = V.fromList ["tokens", "ast", "errors", "outline"]
            }

    case eitherErrorOrResp of
      Left errorMessage -> printf "registration failed due to %s\n" (show errorMessage)
      Right (Port p) ->
        Z.withSocket ctx Z.Pair $ \serviceSocket -> do
          --Z.withSocket ctx Z.Sub $ \configSocket -> do
          Z.connect serviceSocket $ serviceAddress cfg ++ show p
          printf "connected to %s%d\n" (serviceAddress cfg) p

          --Z.connect configSocket (configurationAddress cfg)
          flip finally (deregister cfg "ghc") $ defaultErrorHandler defaultFatalMessager defaultFlushOut $
            runGhc (Just libdir) $ do
              dflags0 <- getSessionDynFlags
              let dflags = gopt_set dflags0 Opt_KeepRawTokenStream
              _ <- setSessionDynFlags dflags

              forever $ do
                rawMsg <- liftIO $ Z.receive serviceSocket
                case eitherDecodeStrict rawMsg of
                  Right msgs ->
                    V.forM_ msgs $ \msg -> do
                      productMessages <- onVersionMessage outlineIcons msg
                      liftIO $ forM_ productMessages $ Z.send serviceSocket [] . BL.toStrict . encode
                  Left err ->
                    liftIO $ printf "Could not decode version message %s\n%s\n" (B.unpack rawMsg) err

register :: ZMQConfig -> RegisterServiceRequest -> IO (Either Text Port)
register cfg request =
  Z.withSocket (zeromqContext cfg) Z.Req $ \registrationSocket -> do
    Z.connect registrationSocket (registrationAddress cfg)
    Z.send registrationSocket [] $ BL.toStrict $ encode request
    str <- Z.receive registrationSocket
    case decodeStrict str of
      Just (RegisterServiceResponse _ (Just p))  -> return (Right p)
      Just (RegisterServiceResponse msg Nothing) -> return (Left msg)
      Nothing -> return $ Left "could not register service"

deregister :: ZMQConfig -> ServiceID -> IO ()
deregister cfg sid =
  Z.withSocket (zeromqContext cfg) Z.Req $ \registrationSocket -> do
    Z.connect registrationSocket (registrationAddress cfg)
    Z.send registrationSocket [] $ BL.toStrict $ encode (DeregisterService sid)

onVersionMessage :: GhcMonad m => OutlineIcons -> VersionMessage -> m [ProductMessage]
onVersionMessage outlineIcons versionMessage = do
  prods <- compile outlineIcons (V.contents versionMessage)
  return $ prods >>= \ps ->
    let tokenMessage = toProductMessage versionMessage "tokens" . toJSON <$> tokens ps
        --astMessage = toProductMessage versionMessage "ast" . astToJSON <$> ast ps
        outlineMessage = toProductMessage versionMessage "outline" . toJSON <$> outline ps
        errorMessage = toProductMessage versionMessage "errors" $ toJSON $ errors ps
    in catMaybes
           [ tokenMessage
           --, astMessage
           , outlineMessage
           , Just errorMessage
           ]

toProductMessage :: VersionMessage -> Product -> Value -> ProductMessage
toProductMessage versionMessage prod contents =
  ProductMessage
    { P.versionId = V.versionId versionMessage
    , P.source = V.source versionMessage
    , P.serviceId = "ghc"
    , P.product = prod
    , P.language = V.language versionMessage
    , P.contents = contents
    , P.dependencies = V.empty
    }

compile :: GhcMonad m => OutlineIcons -> Text -> m [Products]
compile outlineIcons documentText = flip gfinally clearTargets $ do
  let document = fromText documentText
  dflags <- getSessionDynFlags
  srcFile <- liftIO $ newTempName dflags "hs"
  liftIO $ T.writeFile srcFile documentText
  target <- guessTarget srcFile Nothing
  setTargets [target]
  mg <- depanal [] False
  let mods = flattenSCCs (topSortModuleGraph False mg Nothing)
  forM mods $ \modsum -> do
    parsedMod <- handleSourceError (return . Left . srcErrorMessages) (Right <$> parseModule modsum)
    case parsedMod of
      Left err ->
        return Products
          { tokens = Nothing
          , ast = Nothing
          , outline = Nothing
          , errors = encodeErrorMessage dflags document <$> bagToList err
          }
      Right pm -> do
        typeErrors <- handleSourceError
                      (return . srcErrorMessages)
                      (typecheckModule pm >> return emptyBag)
        liftIO $ putStrLn $ unlines $ map show $ encodeTokens document pm
        --liftIO $ putStrLn $ drawTree $ fmap show $ encodeAST dflags document (unLoc (pm_parsed_source pm))
        --liftIO $ print $ encodeOutline document outlineIcons pm
        return Products
          { tokens = Just $ encodeTokens document pm
          , ast = Just $ encodeAST dflags document (unLoc (pm_parsed_source pm))
          , outline = Just $ encodeOutline document outlineIcons pm
          , errors = encodeErrorMessage dflags document <$> bagToList typeErrors
          }

clearTargets :: GhcMonad m => m ()
clearTargets = do
  dflags <- getSessionDynFlags

  -- cannot use SysTools.cleanTempFiles to remove temporary files, because
  -- it doesn't remove *.hs files.
  liftIO $ cleanAllTempFiles dflags

  setTargets []
  _ <- load LoadAllTargets
  return ()

cleanAllTempFiles :: DynFlags -> IO ()
cleanAllTempFiles dflags = do
  let ref = filesToClean dflags
  fs <- atomicModifyIORef ref $ \fs -> ([],fs)
  mapM_ removeFile fs

data Products = Products { tokens :: Maybe [Value]
                         , ast :: Maybe (Tree Value)
                         , outline :: Maybe Outline
                         , errors :: [Value] } deriving Show
