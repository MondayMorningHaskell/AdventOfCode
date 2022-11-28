{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Void (Void)
import Data.Text (Text, pack)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec (some, many, MonadParsec (getParserState), ParsecT, try, eof)
import Text.Megaparsec.Char (eol)
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Utils (rpdl, parse2DMapSpace, parse2DMapSpaceMany)

example1 :: [String]
example1 =
  [ "22 13 17 11  0"
  , " 8  2 23  4 24"
  , "21  9 14 16  7"
  , " 6 10  3 18  5"
  , " 1 12 20 15 19"
  , ""
  , " 3 15  0  2 22"
  , " 9 18 13 17  5"
  , "19  8  7 25 23"
  , "20 11 10 24  4"
  , "14 21 16 12  6"
  ]

main :: IO ()
main = do
  result1 <- runStdoutLoggingT $ rpdl parse2DMapSpaceMany example1
  defaultMain $ testGroup "Advent of Code Utils Tests"
    [ testCase "Parse 2D Spaced Int Map 1" $ length result1 > 1 @?= True
    ]