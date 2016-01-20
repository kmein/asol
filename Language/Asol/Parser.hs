module Language.Asol.Parser where

import Language.Asol.Expression
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseInstruction :: Parser Instruction
parseInstruction = parsePush <|> parsePop  <|> parseRead <|> parseTop  <|> parsePrint
                             <|> parseMul  <|> parseSub  <|> parseAdd  <|> parseDiv   <|> parseMod
                             <|> parseSwap <|> parseDup  <|> parseEmit
  where
    parsePush  = Push <$> (optional (char '>') *> (read <$> many1 digit))
    parsePop   = Pop <$ char '<'
    parseRead  = Read <$ char '&'
    parseTop   = Top <$> (char '^' *> nat)
    parsePrint = Print <$ char '.'
    parseMul   = Mul <$ char '*'
    parseSub   = Sub <$ char '-'
    parseAdd   = Add <$ char '+'
    parseDiv   = Div <$ char '/'
    parseMod   = Mod <$ char '%'
    parseSwap  = Swap <$ char '~'
    parseDup   = Dup <$ char '='
    parseEmit  = Emit <$ char ';'

parseInstructions :: Parser [Instruction]
parseInstructions = parseInstruction `sepBy` optional spaces
