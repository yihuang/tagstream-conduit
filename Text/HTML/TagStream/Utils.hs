{-# LANGUAGE ViewPatterns #-}
module Text.HTML.TagStream.Utils where

import Data.Word
import Data.Monoid (Monoid(..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable (Storable(peekByteOff))

import Text.HTML.TagStream.Types

splitAccum :: Monoid s => [Token' s] -> (s, [Token' s])
splitAccum [] = (mempty, [])
splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
splitAccum tokens = (mempty, tokens)

peekByteOff' :: Storable a => ForeignPtr b -> Int -> a
peekByteOff' p i = S.inlinePerformIO $ withForeignPtr p $ \p' -> peekByteOff p' i
{-# INLINE peekByteOff' #-}

cons' :: Word8 -> S.ByteString -> S.ByteString
cons' c bs@(S.PS p s l)
  | s>0 && peekByteOff' p (s-1)==c = S.PS p (s-1) (l+1)
  | otherwise = S.cons c bs
{-# INLINE cons' #-}

cons :: Char -> S.ByteString -> S.ByteString
cons = cons' . S.c2w
{-# INLINE cons #-}
