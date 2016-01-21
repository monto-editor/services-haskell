{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Outline where

import           SourceSpan

import           GHC

import           Data.Aeson.TH
import           Data.Maybe
import           Data.Text (Text)

type Icon = FilePath
data Outline = Outline
    { description :: Text
    , identifier :: SourceSpan
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
                  }

getOutlineIcons :: String -> IO OutlineIcons
getOutlineIcons url = OutlineIcons
                  <$> getResource "class.png"
                  <*> getResource "module.png"
                  <*> getResource "private.png"
                  <*> getResource "public.png"
                  <*> getResource "type.png"
  where
    getResource name = return $ url ++ name

encodeOutline :: Document -> OutlineIcons -> ParsedModule -> Outline
encodeOutline doc icons pm =
  let md = unLoc $ pm_parsed_source pm
      maybeModName = hsmodName md
  in Outline
      { description = "compilation-unit"
      , identifier = SourceSpan 0 0
      , icon = ""
      , children = [
         Outline
           { description = "module"
           , identifier = fromMaybe (SourceSpan 0 0) $ lineColumnToOffsetLength doc =<< getLoc <$> maybeModName
           , icon = modul icons
           , children = mapMaybe outlineDecl (hsmodDecls md)
           }
        ]
      }
  where

    outlineDecl :: LHsDecl RdrName -> Maybe Outline
    outlineDecl ldecl = do
      (desc,ico) <- case unLoc ldecl of
        TyClD FamDecl{} -> Just ("type-family", typ icons)
        TyClD SynDecl{} -> Just ("type-alias", typ icons)
        TyClD DataDecl{} -> Just ("data-declaration", typ icons)
        TyClD ClassDecl{} -> Just ("class-declarartion", typ icons)
        SigD TypeSig{} -> Just ("type-signature", public icons)
        _ -> Nothing
      return Outline
        { description = desc
        , identifier = fromJust $ lineColumnToOffsetLength doc $ getLoc ldecl
        , icon = ico
        , children = []
        }
