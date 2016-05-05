module Text.HTML.TagStream.ByteString(
    module Entities.ByteString,
    decodeEntitiesBS,
    ) where

import Entities.ByteString
import           Data.Conduit

decodeEntitiesBS :: Monad m => Conduit Token m Token
decodeEntitiesBS = decodeEntities
