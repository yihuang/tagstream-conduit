{-# LANGUAGE OverloadedStrings #-}
module Str.ByteString(module Str.ByteString, module Data.ByteString.Char8) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Text.Encoding
import qualified Text.XML.Stream.Parse as XML

type Str = ByteString

decodeEntity :: Str -> Maybe Str
decodeEntity entity =
          fmap encodeUtf8
          $ CL.sourceList ["&",entity,";"]
          $= XML.parseBytes def { XML.psDecodeEntities = XML.decodeHtmlEntities }
          $$ XML.content
