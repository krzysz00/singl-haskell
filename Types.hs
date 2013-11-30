module Types (Token(..)) where

data Token = Address !Integer 
           | Dereference !Integer 
           | Output !Integer 
           | Input !Integer
           | Variable !Integer
           | Argument
           | Add Token Token
           | Subtract Token Token
           | Multiply Token Token
           | Divide Token Token
           | Modulo Token Token
           | EqZero Token Token Token
           | LeqZero Token Token Token
           | Loop Token Token
           | Group [Token] 
           | Assign Token [Token] deriving (Eq, Show, Read)
