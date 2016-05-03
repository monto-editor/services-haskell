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
import           Panic
import           SysTools

import           Control.Exception
import           Control.Monad
import           Control.Concurrent

import           Data.Aeson

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Tree

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import           Network.HTTP.Types


import           Options.Applicative hiding (str)

import           System.ZMQ4 (Context)
import qualified System.ZMQ4 as Z
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import           System.Exit
import           System.Directory

import           Text.Printf

import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import qualified Monto.ProductDescription as PD
import           Monto.DeregisterService (DeregisterService(DeregisterService))
import           Monto.RegisterServiceRequest (RegisterServiceRequest(RegisterServiceRequest))
import qualified Monto.RegisterServiceRequest as R
import           Monto.RegisterServiceResponse
import           Monto.Request as Req
import           Monto.ProductDependency
import           Monto.Types hiding (fromText)
import           Monto.SourceMessage (SourceMessage)
import qualified Monto.SourceMessage as S

import           Paths_services_haskell

data Config = Config
               { zeromqContext :: Context
               , serviceAddress :: String
               , registrationAddress :: String
               , configurationAddress :: String
               , resourcePort :: Int
               }

parseConfig :: Context -> Parser Config
parseConfig ctx =
  Config
  <$> pure ctx
  <*> strOption
         ( long "service-address"
        <> metavar "ADDRESS"
        <> help "The address without port on which services listen on" )
  <*> strOption
         ( long "registration"
        <> metavar "ADDRESS"
        <> help "The address on which services register themselves" )
  <*> strOption
         ( long "configuration"
        <> metavar "ADDRESS"
        <> help "The address on which services can be configured" )
  <*> option auto
         ( long "resource-port"
        <> metavar "PORT"
        <> help "The port on which resources get published" )

data Interrupted = Interrupted
  deriving (Eq,Show)

resourceServer :: Config -> FilePath -> IO ()
resourceServer cfg root =
  Warp.run (resourcePort cfg) $ \req respond -> respond $
    Wai.responseFile
       status200
       [("Content-Type", "image/png")]
       (printf "%s/%s" root (T.unpack (head (Wai.pathInfo req))))
       Nothing

main :: IO ()
main =
  Z.withContext $ \ctx -> do
    _ <- installHandler sigINT  (Catch exitSuccess) Nothing
    _ <- installHandler sigTERM (Catch exitSuccess) Nothing

    cfg <- execParser $ info (helper <*> parseConfig ctx)
      ( fullDesc <> header "service-haskell - Monto services for Haskell" )

    outlineIcons <- getOutlineIcons $ printf "http://localhost:%d/" (resourcePort cfg)
    assetDir <- getDataFileName "assets"
    _ <- forkIO $ resourceServer cfg assetDir

    eitherErrorOrResp <- register cfg RegisterServiceRequest
            { R.serviceID = "ghc"
            , R.label = "Glasgow Haskell Compiler"
            , R.description = "The GHC compiler does tokenization, parsing and typechecking"
            , R.options = Nothing
            , R.dependencies = [ SourceDependency "haskell" ]
            , R.products = [ PD.ProductDescription "tokens" "haskell"
                           , PD.ProductDescription "ast" "haskell"
                           , PD.ProductDescription "errors" "haskell"
                           , PD.ProductDescription "outline" "haskell"
                           ]
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
                    Right (Req.Request _ _ (SourceMessage m:_)) -> do
                      productMessages <- onSourceMessage outlineIcons m
                      liftIO $ forM_ productMessages $ Z.send serviceSocket [] . BL.toStrict . encode
                    Right _ ->
                      liftIO $ printf "Request did not contain a source message: %s\n" (B.unpack rawMsg)
                    Left err ->
                      liftIO $ printf "Could not decode request message %s\n%s\n" (B.unpack rawMsg) err

register :: Config -> RegisterServiceRequest -> IO (Either Text Port)
register cfg request =
  Z.withSocket (zeromqContext cfg) Z.Req $ \registrationSocket -> do
    Z.connect registrationSocket (registrationAddress cfg)
    Z.send registrationSocket [] $ BL.toStrict $ encode request
    str <- Z.receive registrationSocket
    case decodeStrict str of
      Just (RegisterServiceResponse _ (Just p))  -> return (Right p)
      Just (RegisterServiceResponse msg Nothing) -> return (Left msg)
      Nothing -> return $ Left "could not register service"

deregister :: Config -> ServiceID -> IO ()
deregister cfg sid =
  Z.withSocket (zeromqContext cfg) Z.Req $ \registrationSocket -> do
    Z.connect registrationSocket (registrationAddress cfg)
    Z.send registrationSocket [] $ BL.toStrict $ encode (DeregisterService sid)

onSourceMessage :: GhcMonad m => OutlineIcons -> SourceMessage -> m [ProductMessage]
onSourceMessage outlineIcons sourceMessage = do
  prods <- compile outlineIcons (S.contents sourceMessage)
  return $ prods >>= \ps ->
    let tokenMessage = toProductMessage sourceMessage "tokens" . toJSON <$> tokens ps
        --astMessage = toProductMessage versionMessage "ast" . astToJSON <$> ast ps
        outlineMessage = toProductMessage sourceMessage "outline" . toJSON <$> outline ps
        errorMessage = toProductMessage sourceMessage "errors" $ toJSON $ errors ps
    in catMaybes
           [ tokenMessage
           --, astMessage
           , outlineMessage
           , Just errorMessage
           ]

toProductMessage :: SourceMessage -> Product -> Value -> ProductMessage
toProductMessage sourceMessage prod contents =
  P.ProductMessage
    { P.id = S.id sourceMessage
    , P.source = S.source sourceMessage
    , P.serviceID = "ghc"
    , P.product = prod
    , P.language = S.language sourceMessage
    , P.contents = contents
    }

compile :: GhcMonad m => OutlineIcons -> Text -> m [Products]
compile outlineIcons documentText = flip gfinally clearTargets $ handleGhcException' $ do
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
          , errors = encodeSourceErrorMessage dflags document <$> bagToList err
          }
      Right pm -> do
        typeErrors <- handleSourceError
                      (return . srcErrorMessages)
                      (typecheckModule pm >> return emptyBag)
        --liftIO $ putStrLn $ unlines $ map show $ encodeTokens document pm
        --liftIO $ putStrLn $ drawTree $ fmap show $ encodeAST dflags document (unLoc (pm_parsed_source pm))
        --liftIO $ print $ encodeOutline document outlineIcons pm
        return Products
          { tokens = Just $ encodeTokens document pm
          , ast = Just $ encodeAST dflags document (unLoc (pm_parsed_source pm))
          , outline = Just $ encodeOutline dflags document outlineIcons pm
          , errors = encodeSourceErrorMessage dflags document <$> bagToList typeErrors
          }
  where
    handleGhcException' = handleGhcException $ \e ->
      return $ return Products
          { tokens = Nothing
          , ast = Nothing
          , outline = Nothing
          , errors = return $ encodeGHCErrorMessage e
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
