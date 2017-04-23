{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Entities where

import Prelude hiding (break, drop, concat, null)

import           Control.Applicative
import           Control.Monad (unless)
import           Data.Char

import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid
import           Control.Arrow (first)

import qualified Data.Conduit.List as CL
import Data.Conduit

import Text.HTML.TagStream.Types

import Str as T
import Builder
import Parser

type Token = Token' Str
type Attr = Attr' Str

{--
 - match quoted string, can fail.
 -}
quoted :: Char -> Parser Str
quoted q = append <$> takeTill (in2 ('\\',q))
                    <*> ( char q *> pure ""
                      <|> char '\\' *> atLeast 1 (quoted q) )

quotedOr :: Parser Str -> Parser Str
quotedOr p = maybeP (satisfy (in2 ('"','\''))) >>=
             maybe p quoted

{--
 - attribute value, can't fail.
 -}
attrValue :: Parser Str
attrValue = quotedOr $ takeTill ((=='>') ||. isSpace)

{--
 - attribute name, at least one char, can fail when meet tag end.
 - might match self-close tag end "/>" , make sure match `tagEnd' first.
 -}
attrName :: Parser Str
attrName = quotedOr $
               cons <$> satisfy (/='>')
                    <*> takeTill (in3 ('/','>','=') ||. isSpace)

{--
 - tag end, return self-close or not, can fail.
 -}
tagEnd :: Parser Bool
tagEnd = char '>' *> pure False
     <|> string "/>" *> pure True

{--
 - attribute pair or tag end, can fail if tag end met.
 -}
attr :: Parser Attr
attr = (,) <$> attrName <* skipSpace
           <*> ( boolP (char '=') >>=
                 cond (skipSpace *> attrValue)
                      (pure "")
               )

{--
 - all attributes before tag end. can't fail.
 -}
attrs :: Parser ([Attr], Bool)
attrs = loop []
  where
    loop acc = skipSpace *> (Left <$> tagEnd <|> Right <$> attr) >>=
               either
                 (return . (reverse acc,))
                 (loop . (:acc))

{--
 - comment tag without prefix.
 -}
comment :: Parser Token
comment = Comment <$> comment'
  where comment' =   append <$> takeTill (=='-')
                            <*> ( string "-->" *> return ""
                              <|> atLeast 1 comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> (   cons <$> satisfy (not . ((=='-') ||. isSpace))
                       <*> takeTill ((=='>') ||. isSpace)
                       <* skipSpace )
          <*> takeTill (=='>') <* char '>'

{--
 - parse a tag, can fail.
 -}
tag :: Parser Token
tag = do
    t <-     string "/" *> return TagTypeClose
         <|> string "!" *> return TagTypeSpecial
         <|> return TagTypeNormal
    case t of
        TagTypeClose ->
            TagClose <$> takeTill (=='>')
            <* char '>'
        TagTypeSpecial -> boolP (string "--") >>=
                          cond comment special
        TagTypeNormal -> do
            name <- takeTill (in3 ('<','>','/') ||. isSpace)
            (as, close) <- attrs
            return $ TagOpen name as close

{--
 - record incomplete tag for streamline processing.
 -}
incomplete :: Parser Token
incomplete = Incomplete . cons '<' <$> takeRest

{--
 - parse text node. consume at least one char, to make sure progress.
 -}
text :: Parser Token
text = Text <$> atLeast 1 (takeTill (=='<'))

-- | A conduit to decode entities from a stream of tokens into a new stream of tokens.
decodeEntities :: Monad m => Conduit Token m Token
decodeEntities =
    start
  where
    start = await >>= maybe (return ()) (\tok -> start' tok >> start)
    start' (Text t) = (yield t >> yieldWhileText) =$= decodeEntities' =$= CL.mapMaybe go
    start' tok = yield tok

    go t
        | t == ""   = Nothing
        | otherwise = Just (Text t)

decodeEntities' :: Monad m => Conduit Str m Str
decodeEntities' =
    loop id
  where
    loop accum = do
        mchunk <- await
        let chunk = accum $ fromMaybe mempty mchunk
            (newStr, remainder) = makeEntityDecoder chunk
        yield newStr
        if isJust mchunk
            then loop (mappend remainder)
            else yield remainder

-- | Yield contiguous text tokens as strings.
yieldWhileText :: Monad m => Conduit Token m Str
yieldWhileText =
    loop
  where
    loop = await >>= maybe (return ()) go
    go (Text t) = yield t >> loop
    go tok = leftover tok

-- | Decode the entities in a string type with a decoder.
makeEntityDecoder :: Str -> (Str, Str)
makeEntityDecoder = first builderToStr . go
  where
    go s =
      case break (=='&') s of
        (_,"") -> (strToBuilder s, "")
        (before,restPlusAmp@(drop 1 -> rest)) ->
          case break (not . (\c -> isNameChar c || c == '#')) rest of
            (_,"") -> (strToBuilder before, restPlusAmp)
            (entity,after) -> (before1 <> before2, after')
              where
                before1 = strToBuilder before
                (before2, after') =
                  case mdecoded of
                    Nothing -> first ((strToBuilder "&" <> strToBuilder entity) <>) (go after)
                    Just (strToBuilder -> decoded) ->
                      case uncons after of
                        Just (';',validAfter) -> first (decoded <>) (go validAfter)
                        Just (_invalid,_rest) -> first (decoded <>) (go after)
                        Nothing -> (mempty, s)
                mdecoded =
                  if entity == mempty
                     then Nothing
                     else decodeEntity entity

-- | Is the character a valid Name starter?
isNameStart :: Char -> Bool
isNameStart c =
  c == ':' ||
  c == '_' ||
  isAsciiUpper c ||
  isAsciiLower c ||
  (c >= '\xC0' && c <= '\xD6') ||
  (c >= '\xD8' && c <= '\xF6') ||
  (c >= '\xF8' && c <= '\x2FF') ||
  (c >= '\x370' && c <= '\x37D') ||
  (c >= '\x37F' && c <= '\x1FFF') ||
  (c >= '\x200C' && c <= '\x200D') ||
  (c >= '\x2070' && c <= '\x218F') ||
  (c >= '\x2C00' && c <= '\x2FEF') ||
  (c >= '\x3001' && c <= '\xD7FF') ||
  (c >= '\xF900' && c <= '\xFDCF') ||
  (c >= '\xFDF0' && c <= '\xFFFD') ||
  (c >= '\x10000' && c <= '\xEFFFF')

-- | Is the character valid in a Name?
isNameChar :: Char -> Bool
isNameChar c =
  c == '-' ||
  c == '.' ||
  c == '\xB7' ||
  isDigit c ||
  isNameStart c ||
  (c >= '\x0300' && c <= '\x036F') ||
  (c >= '\x203F' && c <= '\x2040')

token :: Parser Token
token = char '<' *> (tag <|> incomplete)
    <|> text

{--
 - treat script tag specially, can't fail.
 -}
tillScriptEnd :: Token -> Parser [Token]
tillScriptEnd t = reverse <$> loop [t]
              <|> (:[]) . Incomplete . append script <$> takeRest
  where
    script = builderToStr $ showToken id t
    loop acc = (:acc) <$> scriptEnd
           <|> (text >>= loop . (:acc))
    scriptEnd = string "</script>" *> return (TagClose "script")

html :: Parser [Token]
html = tokens <|> pure []
  where
    tokens :: Parser [Token]
    tokens = do
        t <- token
        case t of
            (TagOpen name _ close)
              | not close && name=="script"
                -> (++) <$> tillScriptEnd t <*> html
            _ -> (t:) <$> html

decode :: Str -> Either String [Token]
decode = fmap go . parseOnly html
  where
    go tokens = runIdentity $ mapM_ yield tokens $$ decodeEntities =$ CL.consume

{--
 - Utils {{{
 -}

atLeast :: Int -> Parser Str -> Parser Str
atLeast 0 p = p
atLeast n p = cons <$> anyChar <*> atLeast (n-1) p

cond :: a -> a -> Bool -> a
cond a1 a2 b = if b then a1 else a2

(||.) :: Applicative f => f Bool -> f Bool -> f Bool
(||.) = liftA2 (||)

in2 :: Eq a => (a,a) -> a -> Bool
in2 (a1,a2) a = a==a1 || a==a2

in3 :: Eq a => (a,a,a) -> a -> Bool
in3 (a1,a2,a3) a = a==a1 || a==a2 || a==a3

boolP :: Parser a -> Parser Bool
boolP p = p *> pure True <|> pure False

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = Just <$> p <|> return Nothing

splitAccum :: Monoid s => [Token' s] -> (s, [Token' s])
splitAccum [] = (mempty, [])
splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
splitAccum tokens = (mempty, tokens)
-- }}}

-- {{{ encode tokens
cc :: [Str] -> Builder
cc = mconcat . map strToBuilder

showToken :: (Str -> Str) -> Token -> Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr as
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> Str
    showAttr (key, value) = concat $ [" ", key, hl "=\""] ++ map escape (unpack value) ++ [hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = singleton c
showToken hl (TagClose name) = cc [hl "</", name, hl ">"]
showToken _ (Text s) = strToBuilder s
showToken hl (Comment s) = cc [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = cc [hl "<!", name, " ", s, hl ">"]
showToken _ (Incomplete s) = strToBuilder s
-- }}}

-- {{{ Stream
tokenStream :: Monad m
#if MIN_VERSION_conduit(1, 0, 0)
            => Conduit Str m Token
#else
            => GInfConduit Str m Token
#endif
tokenStream =
    loop T.empty =$= decodeEntities
  where
#if MIN_VERSION_conduit(1, 0, 0)
    loop accum = await >>= maybe (close accum ()) (push accum)
#else
    loop accum = awaitE >>= either (close accum) (push accum)
#endif

    push accum input =
        case parseOnly html (accum `append` input) of
            Right (splitAccum -> (accum', tokens)) -> mapM_ yield tokens >> loop accum'
            Left err -> fail err

    close s r = do
        unless (null s) $ yield $ Text s
        return r
