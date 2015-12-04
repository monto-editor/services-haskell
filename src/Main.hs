{-# LANGUAGE OverloadedStrings #-}

import GHC hiding (Type)
import GHC.Paths ( libdir )
import DynFlags
import MonadUtils
import IOEnv
import Digraph
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V
import Text.Printf
import Data.Tree
import Data.Generics
import Outputable

main :: IO ()
main = do
  astAndType <- defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget "test.hs" Nothing
      setTargets [target]
      mg <- depanal [] False
      let mods = flattenSCCs (topSortModuleGraph False mg Nothing)
      forM mods $ \modsum -> do
        ast <- parseModule modsum
        typ <- typecheckModule ast
        return (encodeAST dflags (unLoc (pm_parsed_source ast)), typ)
  putStrLn $ drawTree $ fmap show $ fst $ head astAndType
  return ()

encodeAST :: Data a => DynFlags -> a -> Tree Value
encodeAST dflags = encodeDefault
            `extQ` encodeSrcSpan
            `extQ` encodeHsString
            `extQ` encodeRdrName
           `ext1Q` encodeList
           `ext2Q` encodeLocated
  where
    encodeDefault :: Data a => a -> Tree Value
    encodeDefault a = Node (String (T.pack (show (toConstr a)))) (gmapQ (encodeAST dflags) a)

    encodeList :: Data a => [a] -> Tree Value
    encodeList as = Node "List" $ map (encodeAST dflags) as

    encodeRdrName :: RdrName -> Tree Value
    encodeRdrName name = Node (encodePretty name) []

    encodeHsString :: HsLit -> Tree Value
    encodeHsString (HsString str _) = Node "HsString" [Node (String (T.pack str)) []]
    encodeHsString lit = encodeDefault lit

    encodeLocated :: (Data l,Data e) => GenLocated l e -> Tree Value
    encodeLocated (L l e) =
      Node "Located"
        [ encodeAST dflags l
        , encodeAST dflags e
        ]

    encodeSrcSpan :: SrcSpan -> Tree Value
    encodeSrcSpan (RealSrcSpan sp) = Node (object
      [ "start_line" .= srcSpanStartLine sp
      , "start_column" .= srcSpanStartCol sp
      , "end_line" .= srcSpanEndLine sp
      , "end_column" .= srcSpanEndCol sp
      ]) []

    encodePretty str = String (T.pack (show (runSDoc (ppr str) (initSDocContext dflags defaultUserStyle))))
