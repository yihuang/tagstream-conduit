module Builder.ByteString(module Builder.ByteString, module Blaze.ByteString.Builder) where

import Data.ByteString (ByteString)
import Blaze.ByteString.Builder

builderToStr :: Builder -> ByteString
builderToStr = toByteString

strToBuilder :: ByteString -> Builder
strToBuilder = fromByteString
