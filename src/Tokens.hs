{-# LANGUAGE OverloadedStrings #-}
module Tokens where

import           Prelude hiding (length,exp)

import           SourceSpan

import           GHC

import           Control.Monad

import           Data.Aeson hiding (encode)
import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import           Data.Sequence (Seq,(><))
import qualified Data.Sequence as S
import           Data.Foldable (toList)
import           Data.Ord
import           Data.Generics

encodeTokens :: Document -> ParsedModule -> [Value]
encodeTokens doc parsedModule =
  let (keywords,comments) = pm_annotations parsedModule
      tokenList = S.fromList [ encodeToken sp (keywordCategory annKey)
                             | ((_,annKey),srcSpans) <- M.toList keywords
                             , sp <- srcSpans
                             ]
      commentList = encodeComment <$> joinLists (M.elems comments)
  in toList $ fmap snd $ S.sortBy (comparing fst) $ tokenList >< commentList >< topLevel

  where

    joinLists :: [[a]] -> Seq a
    joinLists = join . S.fromList . fmap S.fromList

    encodeToken :: SrcSpan -> Text -> (SourceSpan,Value)
    encodeToken srcSpan category =
      let sp = lineColumnToOffsetLength doc srcSpan
      in case sp of
           Just s ->
             (s,
             object
               [ "offset"   .= offset s
               , "length"   .= length s
               , "category" .= category
               ])
           Nothing ->
             error "cannot encode token due to unhelpful source span"

    keywordCategory :: AnnKeywordId -> Text
    keywordCategory k =
      case k of
        AnnAs -> keyword
        AnnAt -> operator
        AnnBang -> operator
        AnnBackquote -> delimiter
        AnnBy -> keyword
        AnnCase -> conditional
        AnnClass -> structure
        AnnClose -> meta
        AnnCloseC -> parenthesis
        AnnCloseP -> parenthesis
        AnnCloseS -> parenthesis
        AnnColon -> operator
        AnnComma -> delimiter
        AnnCommaTuple -> delimiter
        AnnDarrow -> typ
        AnnData -> structure
        AnnDcolon -> typ
        AnnDefault -> keyword
        AnnDeriving -> keyword
        AnnDo -> statement
        AnnDot -> operator
        AnnDotdot -> operator
        AnnElse -> conditional
        AnnEqual -> operator
        AnnExport -> statement
        AnnFamily -> structure
        AnnForall -> typ
        AnnForeign -> typ
        AnnFunId -> identifier
        AnnGroup -> keyword
        AnnHeader -> meta
        AnnHiding -> keyword
        AnnIf -> conditional
        AnnImport -> keyword
        AnnIn -> keyword
        AnnInfix -> keyword
        AnnInstance -> typ
        AnnLam -> operator
        AnnLarrow -> operator
        AnnLet -> keyword
        AnnMdo -> keyword
        AnnMinus -> operator
        AnnModule -> keyword
        AnnNewtype -> typ
        AnnName -> identifier
        AnnOf -> keyword
        AnnOpen -> meta
        AnnOpenC -> parenthesis
        AnnOpenP -> parenthesis
        AnnOpenPE -> parenthesis
        AnnOpenPTE -> parenthesis
        AnnOpenS -> parenthesis
        AnnPackageName -> identifier
        AnnPattern -> keyword
        AnnProc -> keyword
        AnnQualified -> keyword
        AnnRarrow -> operator
        AnnRec -> keyword
        AnnRole -> structure
        AnnSafe -> keyword
        AnnSemi -> delimiter
        AnnSimpleQuote -> character
        AnnStatic -> keyword
        AnnThen -> conditional
        AnnThIdSplice -> meta
        AnnThIdTySplice -> meta
        AnnThTyQuote -> meta
        AnnTilde -> operator
        AnnTildehsh -> operator
        AnnType -> typ
        AnnUnit -> typ
        AnnUsing -> keyword
        AnnVal -> constant
        AnnValStr -> string
        AnnVbar -> operator
        AnnWhere -> keyword
        Annlarrowtail -> operator
        Annrarrowtail -> operator
        AnnLarrowtail -> operator
        AnnRarrowtail -> operator
        AnnEofPos -> whitespace

    encodeComment :: Located AnnotationComment -> (SourceSpan,Value)
    encodeComment com =
      encodeToken (getLoc com) comment

    topLevel :: Seq (SourceSpan,Value)
    topLevel = extractTokensFromAST (parsedSource parsedModule)

    extractTokensFromAST :: Data a => a -> Seq (SourceSpan,Value)
    extractTokensFromAST = gmapQl mappend mempty extractTokensFromAST
                    `extQ` encodeModuleName
                    `extQ` encodeType
                    `extQ` encodeTypeDecl
                    `extQ` encodeSignature
                    `extQ` encodeExpr

    extractTokensFromSubTerms :: Data a => a -> Seq (SourceSpan,Value)
    extractTokensFromSubTerms = gmapQl mappend mempty extractTokensFromAST

    encode :: Text -> Located a -> Seq (SourceSpan,Value)
    encode cat name = S.singleton $ encodeToken (getLoc name) cat

    encodeModuleName :: Located ModuleName -> Seq (SourceSpan,Value)
    encodeModuleName = encode identifier

    encodeTypeDecl :: TyClDecl RdrName -> Seq (SourceSpan,Value)
    encodeTypeDecl decl = encode typ $ case decl of
      FamDecl famDecl -> fdLName famDecl
      SynDecl name _ _ _ -> name
      DataDecl name _ _ _ -> name
      ClassDecl _ name _ _ _ _ _ _ _ _ -> name

    encodeSignature :: Sig RdrName -> Seq (SourceSpan,Value)
    encodeSignature sig = case sig of
      TypeSig name t1 t2 -> (encode identifier =<< S.fromList name) >< extractTokensFromAST (t1,t2)
      PatSynSig name t1 t2 t3 t4 -> encode identifier name >< extractTokensFromAST (t1,t2,t3,t4)
      GenericSig name t -> (encode identifier =<< S.fromList name) >< extractTokensFromAST t
      InlineSig name t -> encode identifier name >< extractTokensFromAST t
      SpecSig name t1 t2 -> encode typ name >< extractTokensFromAST (t1,t2)
      _ -> extractTokensFromSubTerms sig

    encodeType :: Located (HsType RdrName) -> Seq (SourceSpan,Value)
    encodeType = encode typ

    encodeExpr :: Located (HsExpr RdrName) -> Seq (SourceSpan,Value)
    encodeExpr exp = case unLoc exp of
      HsVar {} -> encode identifier exp
      HsLit {} -> encode constant exp
      HsOverLit {} -> encode constant exp
      _ -> extractTokensFromSubTerms exp

    comment = "comment"
    parenthesis = "parenthesis"
    conditional = "conditional"
    typ = "type"
    delimiter = "delimiter"
    string = "string"
    structure = "class"
    identifier = "identifier"
    constant = "constant"
    meta = "meta"
    statement = "statement"
    keyword = "keyword"
    operator = "operator"
    character = "character"
    whitespace = "whitespace"
