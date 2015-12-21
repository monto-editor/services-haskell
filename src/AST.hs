{-# LANGUAGE OverloadedStrings #-}
module AST where

import           SourceSpan
import           Util

import           GHC

import           Data.Aeson
import           Data.Generics
import qualified Data.Text as T
import           Data.Tree

encodeAST :: Data a => DynFlags -> Document -> a -> Tree Value
encodeAST dflags doc = encodeDefault
            `extQ` encodeSrcSpan
            `extQ` encodeHsString
            `extQ` encodeRdrName
           `ext1Q` encodeList
           `ext2Q` encodeLocated
  where
    encodeDefault :: Data a => a -> Tree Value
    encodeDefault a = Node (String (T.pack (show (toConstr a)))) (gmapQ (encodeAST dflags doc) a)

    encodeList :: Data a => [a] -> Tree Value
    encodeList as = Node "List" $ map (encodeAST dflags doc) as

    encodeRdrName :: RdrName -> Tree Value
    encodeRdrName name = Node (encodePretty name) []

    encodeHsString :: HsLit -> Tree Value
    encodeHsString (HsString str _) = Node "HsString" [Node (String (T.pack str)) []]
    encodeHsString lit = encodeDefault lit

    encodeLocated :: (Data l,Data e) => GenLocated l e -> Tree Value
    encodeLocated (L l e) =
      Node "Located"
        [ encodeAST dflags doc l
        , encodeAST dflags doc e
        ]

    encodePretty = toJSON . sdocToText dflags

    encodeSrcSpan :: SrcSpan -> Tree Value
    encodeSrcSpan = singleton . toJSON . lineColumnToOffsetLength doc

    singleton a = Node a []

astToJSON :: Tree Value -> Value
astToJSON (Node n chlds) = toJSON $ n : map astToJSON chlds
