module Parser.ByteString (module Parser.ByteString, module Data.Attoparsec.ByteString.Char8) where

import Data.ByteString
import Data.Attoparsec.ByteString.Char8

takeRest :: Parser ByteString
takeRest = takeByteString
