{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import Data.Text (pack, unpack)
import Tasks.GADT_1.GADTExpr
import Text.Parsec.Char (char, digit, oneOf, satisfy, space, string)
import Text.Parsec.Combinator (between, many1)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Prim (ParsecT, Stream, many, parseTest, try, (<|>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Token

-- `*>`  discards fst
-- `<*`  discards snd
-- `<$>` infix fmap
-- `<*>` sequential application

intTokenP :: Parser [Char] --Stream s m Char => ParsecT s u m [Char]
intTokenP = many1 digit

boolTokenP :: Parser Char --Stream s m Char => ParsecT s u m Char
boolTokenP = oneOf "TF"

spaceSkipP :: Parser a -> Parser a
spaceSkipP = try . (between (many space) (many space))

braceSkipP :: Parser a -> Parser a
braceSkipP = between (char '(') (char ')')

iLitP :: Parser (Lit Int)
iLitP = (ILit . read) <$> spaceSkipP intTokenP

bLitP :: Parser (Lit Bool)
bLitP = (BLit . (== 'T')) <$> (\x -> braceSkipP x <|> x) (spaceSkipP boolTokenP)

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = Add <$> ((tryThem [iiLitP]) <* char '+') <*> parse

subP :: Parser (Expr Int)
subP = Sub <$> ((tryThem [iiLitP]) <* char '-') <*> parse

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = And <$> ((tryThem [bbLitP, leqP]) <* string "&&") <*> parse

orP :: Parser (Expr Bool)
orP = Or <$> ((tryThem [bbLitP, leqP]) <* string "||") <*> parse

instance MyParse Int where
  variants = [addP, subP, iiLitP]
instance MyParse Bool where
  variants = [orP, andP, leqP, bbLitP]


class MyParse a where
  variants :: [Parser (Expr a)]
  parse    :: Parser (Expr a)
  tryThem  :: [Parser (Expr a)] -> Parser (Expr a)

  parse = tryThem variants

  tryThem pasrsers = matcher $ ([spaceSkipP] <*> pasrsers) ++ ([\p -> spaceSkipP $ braceSkipP $ spaceSkipP p] <*> variants) where
    matcher [x]    = x
    matcher (x:xs) = x <|> matcher xs
