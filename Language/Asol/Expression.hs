module Language.Asol.Expression where

data Instruction = Push Integer
                 | Top Int
                 | Pop
                 | Print
                 | Read
                 | Emit
                 | Mul
                 | Sub
                 | Add
                 | Div
                 | Mod
                 | Pow
                 | Fact
                 | Swap
                 | Dup
                 | ShowStack
                 deriving (Eq,Show)
