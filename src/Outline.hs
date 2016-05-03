{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Outline where

import           SourceSpan
import           Util

import           GHC
import           Outputable

import           Data.Aeson.TH
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

type Icon = FilePath
data Outline = Outline
    { label :: Text
    , link :: SourceSpan
    , icon :: Icon
    , children :: [Outline]
    } deriving Show
$(deriveJSON defaultOptions ''Outline)

data OutlineIcons = OutlineIcons
                  { klass   :: Icon
                  , modul   :: Icon
                  , private :: Icon
                  , public  :: Icon
                  , typ     :: Icon
                  , dataDecl :: Icon
                  , typeSynonym :: Icon
                  , instanceDecl :: Icon
                  }

getOutlineIcons :: String -> IO OutlineIcons
getOutlineIcons url = OutlineIcons
                  <$> getResource "class.png"
                  <*> getResource "module.png"
                  <*> getResource "private.png"
                  <*> getResource "public.png"
                  <*> getResource "type.png"
                  <*> getResource "data-decl.png"
                  <*> getResource "type-synonym.png"
                  <*> getResource "instance-decl.png"
  where
    getResource name = return $ url ++ name

encodeOutline :: DynFlags -> Document -> OutlineIcons -> ParsedModule -> Outline
encodeOutline dflags doc icons pm =
  let md = unLoc $ pm_parsed_source pm
      maybeModName = hsmodName md
  in Outline
      { label = "compilation-unit"
      , link = SourceSpan 0 0
      , icon = ""
      , children = case maybeModName of
           Just modName ->
             Outline
             { label = T.pack $ moduleNameString $ unLoc modName
             , link = fromMaybe (SourceSpan 0 0) $ lineColumnToOffsetLength doc $ getLoc modName
             , icon = modul icons
             , children = []
             } : concatMap outlineDecl (hsmodDecls md)
           Nothing -> concatMap outlineDecl (hsmodDecls md)
      }
  where

    outlineDecl :: LHsDecl RdrName -> [Outline]
    outlineDecl ldecl =
      case unLoc ldecl of
--        TyClD FamDecl{} -> Just ("type-family", typ icons)
        TyClD SynDecl{tcdLName = name, tcdRhs = ty} -> return $ Outline (sdocToText dflags (ppr name <> text " = " <> ppr ty)) lnk (typeSynonym icons) []
        InstD (ClsInstD ClsInstDecl {cid_poly_ty = ty}) -> return $ Outline (sdocToText dflags ty) lnk (instanceDecl icons) []
        TyClD DataDecl{tcdLName = name} -> return $ Outline (sdocToText dflags name) lnk (dataDecl icons) []
        TyClD ClassDecl{tcdLName = name, tcdSigs = sigs} -> return $ Outline  (sdocToText dflags name) lnk (klass icons) (outlineSig =<< sigs)
        SigD sig -> outlineSig (sig <$ ldecl)
        _ -> []

      where
        lnk = fromMaybe (SourceSpan 0 0) $ lineColumnToOffsetLength doc $ getLoc ldecl

    outlineSig :: LSig RdrName -> [Outline]
    outlineSig sig = do
      (lab,ico,children') <- case unLoc sig of
        (TypeSig names tp _) -> do
          name <- names
          return (sdocToText dflags $ ppr (unLoc name) <> text " :: " <> ppr tp, public icons,[])
        _ -> []
      return Outline
        { label = lab
        , link = fromMaybe (SourceSpan 0 0) $ lineColumnToOffsetLength doc $ getLoc sig
        , icon = ico
        , children = children'
        }
