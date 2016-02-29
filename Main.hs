module Main where

import Language.Asol.Interpreter
import Language.Asol.Parser
import Language.Asol.Expression
import Text.ParserCombinators.Parsec (parse)

getAst :: String -> [Instruction]
getAst input =
  case parse parseInstructions "(asol-stdin)" input of
    Left err -> error (show err)
    Right ast -> ast

loop :: [Integer] -> IO [Integer]
loop stack = getLine >>= \input -> execute (getAst input) stack >>= loop

main :: IO ()
main = () <$ loop []
