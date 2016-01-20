module Language.Asol.Expression where

data Instruction = Push Int | Top Int | Pop
                 | Print | Read | Emit
                 | Mul | Sub | Add | Div | Mod
                 | Swap | Dup
                 deriving (Eq, Show)
