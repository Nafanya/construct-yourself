{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text

main :: IO ()
main = hspec $ do
    describe "Task 1 test" parserTest
    describe "Task 1 - extended with or and sub test suite" extendedTest
    describe "Task 1 - my special testsuite" myTest

parserTest :: SpecWith ()
parserTest = do
  it "#1" $ check parse "42" $ i 42
  it "#2" $ check parse "(42) " $ i 42
  it "#3" $ check parse "(42 ) " $ i 42
  it "#4" $ check parse "(   42 ) " $ i 42
  it "#5" $ check parse "(  T ) " $ b True
  it "#6" $ check parse "42 + 41" $ i 42 `Add` i 41
  it "#7" $ check parse "42 < 41" $ i 42 `Leq` i 41
  it "#8" $ check parse "(42 < 41)" $ i 42 `Leq` i 41
  it "#9" $ check parse "T" $ b True
  it "#10" $ check parse "F" $ b False
  it "#11" $ check parse "(F)" $ b False
  it "#12" $ check parse "T && T" $ b True `And` b True

  it "#13" $ checkWithEval parse "T && (100 < 42)" $ BLit False
  it "#14" $ checkWithEval parse "T && F" $ BLit False
  it "#15" $ checkWithEval parse "3 < 4" $ BLit True
  it "#16" $ checkWithEval parse "3 + 42" $ ILit 45
  it "#17" $ checkWithEval parse "3 + 2 + 1" $ ILit 6
  it "#18" $ checkWithEval parse "(3 + 2) + 1" $ ILit 6
  it "#19" $ checkWithEval parse "3 + 42 < 43" $ BLit False
  it "#20" $ checkWithEval parse "42 + 2 < 49 && T" $ BLit True

extendedTest = do
  it "#1" $ check parse "42 - 41" $ i 42 `Sub` i 41
  it "#2" $ check parse "(42 - 41)" $ i 42 `Sub` i 41
  it "#3" $ check parse "T || T" $ b True `Or` b True
  it "#4" $ check parse "T || F" $ b True `Or` b False

  it "#5" $ checkWithEval parse "T || F" $ BLit True
  it "#6" $ checkWithEval parse "T || (100 < 42)" $ BLit True
  it "#7" $ checkWithEval parse "F || F" $ BLit False
  it "#8" $ checkWithEval parse "43 - 42" $ ILit 1
  it "#9" $ checkWithEval parse "3 + 2 - 1" $ ILit 4
  it "#10" $ checkWithEval parse "(3 - 2) + 1" $ ILit 2
  it "#11" $ checkWithEval parse "45 - 1 < 43" $ BLit False
  it "#12" $ checkWithEval parse "42 + 2 < 49 || F" $ BLit True

myTest :: SpecWith ()
myTest = do
  it "#1" $ checkWithEval parse "((1 + 2) + (1 + 2 + (2))) < 49 && F" $ BLit False
  it "#2" $ checkWithEval parse "1 < 49 && F" $ BLit False
  it "#3" $ check parse "1<49&&F" $ (i 1 `Leq` i 49) `And` b False

check :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
check parser inputStr result =
  TP.parse parser "parser" inputStr `shouldBe` Right result

checkWithEval :: Show a => Parser (Expr a) -> Text -> Lit a -> Expectation
checkWithEval parser inputStr result =
  expr <$> TP.parse parser "parser" inputStr `shouldBe` Right result
