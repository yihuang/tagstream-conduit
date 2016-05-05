{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Text.HTML.TagStream.Text(
    module Entities.Text,
    decodeEntitiesText,
    tokenStreamBS,
    ) where

import           Control.Applicative
import           Control.Monad (unless, when, liftM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.Char
import qualified Data.Conduit.List as CL
import           Data.Default
import           Prelude hiding (mapM)

import qualified Data.Attoparsec.ByteString.Char8 as S
import           Data.Attoparsec.Text
import           Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Conduit
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import           Data.Traversable (mapM)
import qualified Text.XML.Stream.Parse as XML
#if MIN_VERSION_conduit(1, 0, 0)
#else
import           Data.Conduit.Internal (pipeL)
#endif
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Text as C

import qualified Text.HTML.TagStream.ByteString as S
import           Text.HTML.TagStream.Types

import Entities.Text

decodeEntitiesText :: Monad m => Conduit Token m Token
decodeEntitiesText = decodeEntities

-- | like `tokenStream', but it process `ByteString' input, decode it according to xml version tag.
--
-- Only support utf-8 and iso8859 for now.
tokenStreamBS :: MonadThrow m
#if MIN_VERSION_conduit(1, 0, 0)
              => Conduit ByteString m Token
#else
              => GLInfConduit ByteString m Token
#endif
tokenStreamBS = do
    -- try to peek the first tag to find the xml encoding.
    tk <- C.sinkParser (skipBOM *> S.skipSpace *> S.char '<' *> S.tag)

    let (mencoding, yieldToken) =
          case tk of
            (TagOpen "?xml" as _) ->
                (lookup "encoding" as, False)
            _ -> (Nothing, True)

    let codec = fromMaybe C.utf8 (mencoding >>= getCodec . CI.mk)

    when yieldToken $ lift (mapM (decodeBS codec) tk) >>= yield

#if MIN_VERSION_conduit(1, 0, 0)
    C.decode codec =$= tokenStream
#else
    C.decode codec `pipeL` tokenStream
#endif
  where
    skipBOM :: S.Parser ()
    skipBOM =
        ( S.string "\xff\xfe"
          <|> S.string "\xef\xbb\xbf"
        ) *> return ()
        <|> return ()

    getCodec :: CI.CI ByteString -> Maybe C.Codec
    getCodec c =
        case c of
            "utf-8"   -> Just C.utf8
            "utf8"    -> Just C.utf8
            "iso8859" -> Just C.iso8859_1
            _         -> Nothing

    --decodeBS :: C.Codec -> ByteString -> m Text
    decodeBS codec bs = liftM T.concat $ yield bs $= C.decode codec $$ C.consume
-- }}}
