module Language.Asol.Interpreter where

import Control.Monad.State
import Language.Asol.Expression
import Data.Char (chr)

type AsolEvent = StateT Stack IO
type Stack = [Int]

stackop :: Int -> AsolEvent a -> AsolEvent a
stackop n op = do
  stack <- get
  if length stack < n
    then error "Stack underflow"
    else op

stackbinop :: (Int -> Int -> Int) -> AsolEvent ()
stackbinop op = stackop 2 $ modify (\(x:y:xs) -> op y x : xs)

evaluate :: Instruction -> AsolEvent ()
evaluate (Push n) = modify (n:)
evaluate (Top n)  = stackop n $ modify (top n)
  where top n xs = let (as, bs) = splitAt n xs in head bs : as ++ tail bs
evaluate Add      = stackbinop (+)
evaluate Div      = stackbinop div
evaluate Dup      = stackop 1 $ modify (\(x:xs) -> x:x:xs)
evaluate Emit     = stackop 1 $ get >>= \x -> liftIO (putChar $ chr $ head x)
evaluate Mod      = stackbinop mod
evaluate Mul      = stackbinop (*)
evaluate Pop      = stackop 1 $ modify tail
evaluate Print    = stackop 1 $ get >>= \x -> liftIO (print $ head x)
evaluate Read     = (evaluate . Push) =<< fmap read (liftIO getLine)
evaluate Sub      = stackbinop (-)
evaluate Swap     = stackop 2 $ modify (\(x:y:xs) -> y:x:xs)

run :: [Instruction] -> AsolEvent ()
run = mapM_ evaluate

execute :: [Instruction] -> Stack -> IO Stack
execute is = execStateT (run is)
