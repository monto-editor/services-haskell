{-# LANGUAGE OverloadedStrings #-}
module ErrorMessages where

import Prelude hiding (length)

import SourceSpan
import Util

import ErrUtils
import GHC
import Outputable

import Data.Aeson
import Data.Text (Text)

encodeSourceErrorMessage :: DynFlags -> Document -> ErrMsg -> Value
encodeSourceErrorMessage dflags doc errMsg =
  let srcSpan = lineColumnToOffsetLength doc (errMsgSpan errMsg)
  in case srcSpan of
       Just sp -> object
         [ "offset" .= offset sp
         , "length" .= length sp
         , "level"  .= if isWarning errMsg
                       then "warning" :: Text
                       else "error" :: Text
         , "description" .= sdocToText dflags (errMsgShortDoc errMsg)
         , "category" .= ("type" :: Text)
         ]
       Nothing -> error $ "cannot encode error message due to missing source span: "
         ++ show (runSDoc (errMsgShortDoc errMsg) (initSDocContext dflags defaultUserStyle))

encodeGHCErrorMessage :: GhcException -> Value
encodeGHCErrorMessage errMsg =
  object
    [ "offset" .= (0 :: Int)
    , "length" .= (0 :: Int)
    , "level"  .= ("error" :: Text)
    , "description" .= show errMsg
    , "category" .= ("type" :: Text)
    ]
