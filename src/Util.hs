module Util where

import           GHC
import           Outputable

import           Data.Text (Text)
import qualified Data.Text as T

sdocToText :: Outputable a => DynFlags -> a -> Text
sdocToText dflags doc = T.pack (show (runSDoc (ppr doc) (initSDocContext dflags defaultUserStyle)))


instance Outputable SDoc where
  ppr = id
  pprPrec _ sdoc = sdoc
