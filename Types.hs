module Types (Token(..)) where

data Token = Address !Integer 
           | Dereference !Integer 
           | Output !Integer 
           | Input !Integer
           | Argument
           | Add
           | Subtract
           | Multiply
           | Divide
           | Modulo
           | EqZero
           | LeqZero
           | Loop
           | Group [Token] deriving (Eq, Show, Read)
