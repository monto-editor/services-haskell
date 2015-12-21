{-# LANGUAGE TemplateHaskell #-}
module SourceSpan where

import           Prelude hiding (length)

import           GHC

import           Data.Aeson.TH
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T

type Offset = Int
type Length = Int

data SourceSpan = SourceSpan
  { offset :: Offset
  , length :: Length
  } deriving (Show,Eq,Ord)
$(deriveJSON defaultOptions ''SourceSpan)

-- | This type is used to translate line and column based
-- to offset and length based position information.
-- The document text is split up by lines to make it faster
-- to find the correct line.
data Document = Document (Vector Offset) deriving Show

fromText :: Text -> Document
fromText txt =
  let ls = T.lines txt
      offs = scanl (\off t -> off + T.length t + 1) 0 ls
  in Document $ V.fromList offs

lineColumnToOffsetLength :: Document -> SrcSpan -> Maybe SourceSpan
lineColumnToOffsetLength (Document vec) (RealSrcSpan sp) =
  let startOff = (vec V.! (srcSpanStartLine sp - 1)) + srcSpanStartCol sp - 1
      endOff = (vec V.! (srcSpanEndLine sp - 1)) + srcSpanEndCol sp - 1
  in Just SourceSpan
     { offset = startOff
     , length = endOff - startOff
     }
lineColumnToOffsetLength _ (UnhelpfulSpan _) = Nothing
