{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack, unpack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, oneOf, satisfy, space,
                                         string, oneOf)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>), Stream, ParsecT)
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

-- `*>`  discards fst
-- `<*`  discards snd
-- `<$>` infix fmap
-- `<*>` sequential application

intTokenP :: Parser [Char] --Stream s m Char => ParsecT s u m [Char]
intTokenP = many1 digit

boolTokenP :: Parser Char --Stream s m Char => ParsecT s u m Char
boolTokenP = oneOf "TF"

spaceSkipP :: Parser a -> Parser a
spaceSkipP p = (many space *> p) <* many space

braceSkipP :: Parser a -> Parser a
braceSkipP = try . between (char '(') (char ')')

iLitP :: Parser (Lit Int)
iLitP = (ILit . read) <$> spaceSkipP intTokenP

bLitP :: Parser (Lit Bool)
bLitP = (BLit . (== 'T')) <$> (\x -> braceSkipP x <|> x) (spaceSkipP boolTokenP)

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = (Add <$> (spaceSkipP (braceSkipP parse) <* char '+') <*> spaceSkipP parse) <|>
       (Add <$> (iiLitP <* char '+') <*> spaceSkipP parse)

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = (And <$> (spaceSkipP (braceSkipP parse) <* string "&&") <*> spaceSkipP parse) <|>
       (And <$> (bbLitP <* string "&&") <*> spaceSkipP parse)

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = try (spaceSkipP addP) <|> braceSkipP parse <|> iiLitP

instance MyParse Bool where
  parse = try (spaceSkipP leqP) <|> try (spaceSkipP andP) <|> braceSkipP parse <|> bbLitP
