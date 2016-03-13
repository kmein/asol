module Language.Asol.Interpreter where

import Control.Monad.State
import Language.Asol.Expression
import Data.Char (chr)

type AsolEvent = StateT Stack IO

type Stack = [Integer]

stackop :: Int -> AsolEvent () -> AsolEvent ()
stackop n op =
  do stack <- get
     if length stack < n
        then error "Stack underflow"
        else op

stackbinop :: (Integer -> Integer -> Integer) -> AsolEvent ()
stackbinop op = stackop 2 $ modify (\(x:y:xs) -> op y x : xs)

evaluate :: Instruction -> AsolEvent ()
evaluate ins =
  case ins of
    Push n -> modify (n :)
    Top n ->
      let top :: Int -> Stack -> Stack
          top i xs =
            let (as,bs) = splitAt i xs
            in head bs : as ++ tail bs
      in stackop n $ modify (top n)
    Add -> stackbinop (+)
    Div -> stackbinop div
    Dup -> stackop 1 $ modify (\(x:xs) -> x : x : xs)
    Emit ->
      stackop 1 $ get >>= \x -> liftIO (putChar $ chr $ fromIntegral $ head x)
    Mod -> stackbinop mod
    Mul -> stackbinop (*)
    Pow -> stackbinop (^)
    Fact ->
      let factorial n = product [1 .. n]
      in stackop 1 $ modify (\(x:xs) -> factorial x : xs)
    Pop -> stackop 1 $ modify tail
    Print -> stackop 1 $ get >>= \x -> liftIO (print $ head x)
    Read -> (evaluate . Push) =<< fmap read (liftIO getLine)
    Sub -> stackbinop (-)
    Swap -> stackop 2 $ modify (\(x:y:xs) -> y : x : xs)
    ShowStack -> get >>= liftIO . print

run :: [Instruction] -> AsolEvent ()
run = mapM_ evaluate

execute :: [Instruction] -> Stack -> IO Stack
execute = execStateT . run
