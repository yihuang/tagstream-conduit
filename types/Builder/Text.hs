{-# LANGUAGE OverloadedStrings #-}
module Builder.Text(
    module Builder.Text, module Data.Text.Lazy.Builder
) where

import qualified Data.Conduit.List as CL
import           Data.Default
import           Prelude hiding (mapM)

import           Data.Conduit
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Text.Lazy.Builder
import qualified Text.XML.Stream.Parse as XML

builderToStr :: Builder -> Text
builderToStr = L.toStrict . toLazyText

strToBuilder :: Text -> Builder
strToBuilder = fromText
