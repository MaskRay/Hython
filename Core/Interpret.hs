{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Hython.Core.Interpret (
  runInterpreter
  ) where

import qualified Data.Map as M
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Lens.Strict
import Hython.Core.Prim
import qualified Hython.Parser.ADT as ADT

class Interpretable t where
  interpret :: t -> Eval Object

instance Interpretable a => Interpretable [a] where
  interpret xs = mapM_ interpret xs >> pass

instance Interpretable ADT.StatementS where
  interpret (ADT.Assign _ to e) = do
    v <- interpret e
    mapM_ (=:v) to
    pass
  interpret (ADT.Conditional _ gs e) = go gs
    where
      go [] = interpret e
      go ((i,t):gs) = do
        c <- interpret i
        if isTrue c
           then interpret t
           else go gs
  interpret (ADT.While _ cond loop els) = callCC $ \k -> do
    let end = k None
        go = do
          c <- interpret cond
          if isTrue c
             then interpret loop >> go
             else unwindPastWhile >> interpret els >> end
    push $ WhileLoop go end
    go
  interpret (ADT.StmtExpr _ e) = interpret e
  interpret (ADT.Continue _) = unwindUptoWhile >>= loop_start
  interpret (ADT.Break _) = unwindPastWhile >>= loop_end
  interpret (ADT.Print _ e c) = go e >> pass
    where
      go e = do
        v <- interpret e
        printObject v
        liftIO . putChar $ if c then ' ' else '\n'

printObject (Integer i) = liftIO . putStr $ show i
printObject (Bool b) = liftIO . putStr $ show b
printObject None = return ()

instance Interpretable ADT.ExprS where
  interpret (ADT.Var _ i) = do
    vs <- access vars
    case M.lookup i vs of
      Nothing -> error "no var"
      Just v -> return v
  interpret (ADT.Int _ i) = return $ Integer i
  interpret (ADT.Bool _ b) = return $ Bool b
  interpret (ADT.None _) = pass
  interpret (ADT.UnaryOp _ op x) = interpret x >>= return . unaryOp op
  interpret (ADT.BinaryOp _ op x y) = do
    x' <- interpret x
    y' <- interpret y
    return $ binaryOp op x' y'

ADT.Var _ i =: x = vars %= M.insert i x
_ =: _ = error "lvalue"

unaryOp ADT.Plus{} (Integer x) = Integer x
unaryOp ADT.Plus{} _ = error "+: unsupported operand type"
unaryOp ADT.Minus{} (Integer x) = Integer (-x)
unaryOp ADT.Minus{} _ = error "-: unsupported operand type"
unaryOp ADT.Not{} (Bool b) = Bool $ not b
unaryOp ADT.Not{} _ = error "not: unsupported operand type"

binaryOp ADT.Plus{} (Integer x) (Integer y) = Integer $ x + y
binaryOp ADT.Plus{} _ _ = error "+: unsupported operand type"
binaryOp ADT.Minus{} (Integer x) (Integer y) = Integer $ x - y
binaryOp ADT.Minus{} _ _ = error "-: unsupported operand type"
binaryOp ADT.Multiply{} (Integer x) (Integer y) = Integer $ x * y
binaryOp ADT.Multiply{} _ _ = error "*: unsupported operand type"
binaryOp ADT.Divide{} (Integer x) (Integer y) = Integer $ x `div` y
binaryOp ADT.Divide{} _ _ = error "/: unsupported operand type"
binaryOp ADT.Modulo{} (Integer x) (Integer y) = Integer $ x `mod` y
binaryOp ADT.Modulo{} _ _ = error "%: unsupported operand type"

binaryOp ADT.Equal{} (Integer x) (Integer y) = Bool $ x == y
binaryOp ADT.Equal{} _ _ = error "==: unsupported operand type"
binaryOp ADT.NotEq{} (Integer x) (Integer y) = Bool $ x /= y
binaryOp ADT.NotEq{} _ _ = error "!=: unsupported operand type"
binaryOp ADT.Less{} (Integer x) (Integer y) = Bool $ x < y
binaryOp ADT.Less{} _ _ = error "<: unsupported operand type"
binaryOp ADT.LessEq{} (Integer x) (Integer y) = Bool $ x <= y
binaryOp ADT.LessEq{} _ _ = error "<=: unsupported operand type"
binaryOp ADT.Greater{} (Integer x) (Integer y) = Bool $ x > y
binaryOp ADT.Greater{} _ _ = error ">: unsupported operand type"
binaryOp ADT.GreaterEq{} (Integer x) (Integer y) = Bool $ x >= y
binaryOp ADT.GreaterEq{} _ _ = error ">=: unsupported operand type"

binaryOp ADT.And{} (Bool x) (Bool y) = Bool $ x && y
binaryOp ADT.And{} _ _ = error "and: unsupported operand type"
binaryOp ADT.Or{} (Bool x) (Bool y) = Bool $ x || y
binaryOp ADT.Or{} _ _ = error "or: unsupported operand type"

runInterpreter toks = runContT (evalStateT (interpret toks) initS) return
