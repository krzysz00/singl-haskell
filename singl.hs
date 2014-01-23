module Main where

import Text.ParserCombinators.Parsec (parse)

import Types
import Parser
import Evaluator

main :: IO ()
main = do
       p <- getContents 
       case parse singl "(input)" p of
         Left e -> print e >> return ()
         Right program -> evaluate program

