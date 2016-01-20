module Main where

import Language.Asol.Interpreter
import Language.Asol.Parser
import Text.ParserCombinators.Parsec (parse)

getAst input =
  case parse parseInstructions "(asol-stdin)" input of
    Left err -> error (show err)
    Right ast -> ast

loop :: [Int] -> IO [Int]
loop stack = do
  input <- getLine
  let ast = getAst input
  newStack <- execute ast stack
  loop newStack

main :: IO ()
main = () <$ loop []
