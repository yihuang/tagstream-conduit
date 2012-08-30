{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
module Text.HTML.TagStream.Text where

import Control.Applicative
import Control.Monad (unless)

import Data.Monoid (mconcat)
import Data.Char (isSpace)
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.Attoparsec.Text
import Data.Conduit (GInfConduit, awaitE, yield)

import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Utils (splitAccum)

type Token = Token' Text
type Attr = Attr' Text

{--
 - match quoted string, can fail.
 -}
quoted :: Char -> Parser Text
quoted q = L.append <$> (L.fromStrict <$> takeTill (in2 ('\\',q)))
                    <*> ( char q *> pure ""
                      <|> char '\\' *> atLeast 1 (quoted q) )

quotedOr :: Parser Text -> Parser Text
quotedOr p = maybeP (satisfy (in2 ('"','\''))) >>=
             maybe p quoted

{--
 - attribute value, can't fail.
 -}
attrValue :: Parser Text
attrValue = quotedOr $ L.fromStrict <$> takeTill ((=='>') ||. isSpace)

{--
 - attribute name, at least one char, can fail when meet tag end.
 - might match self-close tag end "/>" , make sure match `tagEnd' first.
 -}
attrName :: Parser Text
attrName = quotedOr $
             L.cons <$> satisfy (/='>')
                    <*> (L.fromStrict <$> takeTill (in3 ('/','>','=') ||. isSpace))

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
  where comment' = L.append <$> (L.fromStrict <$> takeTill (=='-'))
                            <*> ( string "-->" *> return ""
                              <|> atLeast 1 comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> ( L.cons <$> satisfy (not . ((=='-') ||. isSpace))
                       <*> (L.fromStrict <$> takeTill ((=='>') ||. isSpace))
                       <*  skipSpace )
          <*> (L.fromStrict <$> takeTill (=='>') <* char '>')

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
            TagClose . L.fromStrict <$> takeTill (=='>')
            <* char '>'
        TagTypeSpecial -> boolP (string "--") >>=
                          cond comment special
        TagTypeNormal -> do
            name <- L.fromStrict <$> takeTill (in3 ('<','>','/') ||. isSpace)
            (as, close) <- attrs
            skipSpace
            return $ TagOpen name as close

{--
 - record incomplete tag for streamline processing.
 -}
incomplete :: Parser Token
incomplete = Incomplete . L.cons '<' <$> takeLazyText

{--
 - parse text node. consume at least one char, to make sure progress.
 -}
text :: Parser Token
text = Text <$> atLeast 1 (L.fromStrict <$> takeTill (=='<'))

token :: Parser Token
token = char '<' *> (tag <|> incomplete)
    <|> text

{--
 - treat script tag specially, can't fail.
 -}
tillScriptEnd :: Token -> Parser [Token]
tillScriptEnd t = reverse <$> loop [t]
              <|> (:[]) . Incomplete . L.append script <$> takeLazyText
  where
    script = B.toLazyText $ showToken id t
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

decode :: T.Text -> Either String [Token]
decode = parseOnly html

{--
 - Utils {{{
 -}

atLeast :: Int -> Parser Text -> Parser Text
atLeast 0 p = p
atLeast n p = L.cons <$> anyChar <*> atLeast (n-1) p

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
-- }}}

-- {{{ encode tokens
cc :: [Text] -> B.Builder
cc = mconcat . map B.fromLazyText

showToken :: (Text -> Text) -> Token -> B.Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr as
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> Text
    showAttr (key, value) = L.concat $ [" ", key, hl "=\""] ++ map escape (L.unpack value) ++ [hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = L.singleton c
showToken hl (TagClose name) = cc [hl "</", name, hl ">"]
showToken _ (Text s) = B.fromLazyText s
showToken hl (Comment s) = cc [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = cc [hl "<!", name, " ", s, hl ">"]
showToken _ (Incomplete s) = B.fromLazyText s
-- }}}

-- {{{ Stream
tokenStream :: Monad m => GInfConduit T.Text m Token
tokenStream =
    loop L.empty
  where
    loop accum = awaitE >>= either (close accum) (push accum)

    push accum input =
        case parseOnly html (L.toStrict accum `T.append` input) of
            Right (splitAccum -> (accum', tokens)) -> mapM_ yield tokens >> loop accum'
            Left err -> fail err

    close s r = do
        unless (L.null s) $ yield $ Text s
        return r
-- }}}
