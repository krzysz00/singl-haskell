module Main where

import Text.ParserCombinators.Parsec (parse)

import Types
import Parser

main :: IO ()
main = interact (show . parse singl "(input)")

