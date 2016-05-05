module Parser.Text (module Parser.Text, module Data.Attoparsec.Text) where

import Data.Text
import Data.Attoparsec.Text

takeRest :: Parser Text
takeRest = takeText
