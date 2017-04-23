{-# LANGUAGE OverloadedStrings #-}
module Str.Text(module Str.Text, module Data.Text) where

import qualified Data.Conduit.List as CL
import           Data.Default
import           Prelude hiding (mapM)

import           Data.Conduit
import           Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Text.XML.Stream.Parse as XML

type Str = Text

decodeEntity :: Str -> Maybe Str
decodeEntity entity =
    CL.sourceList ["&",entity,";"]
    $= XML.parseText def { XML.psDecodeEntities = XML.decodeHtmlEntities }
    $= CL.map snd
    $$ XML.content
