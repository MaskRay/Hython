{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Hython.Core.Prim (
  ControlStack(..), Eval, push, pop, initS,
  Object(..), isTopLevel, local_vars, enclosing_vars, global_vars, nesting_lv, control, pass,
  isTrue, unwind, unwindPastWhile, unwindUptoWhile, dumpStack
  ) where

import Control.Monad.State.Strict
import Control.Monad.Cont
import Data.IORef
import Data.Lens.Strict
import qualified Hython.Parser.ADT as ADT
import Hython.Parser.Location
import qualified Data.Map as M

type Procedure = [Object] -> Eval Object
data Object = None | Integer Integer | Bool Bool | Function { object_arity :: Int, object_proc :: !Procedure }
type ObjectRef = IORef Object

data ControlStack =
  EmptyStack
  | WhileLoop
    { loop_start :: Eval Object
    , loop_end :: Eval Object
    , stack_tail :: ControlStack
    }
  | ProcedureCall
    { procedure_return :: Object -> Eval Object
    , stack_tail :: ControlStack
    }

data S = S { _local_vars :: M.Map ADT.IdentS ObjectRef
           , _enclosing_vars :: M.Map ADT.IdentS ObjectRef
           , _global_vars :: M.Map ADT.IdentS ObjectRef
           , _nesting_lv :: Int
           , _control :: ControlStack }

type Eval a = StateT S (ContT Object IO) a

isTopLevel = liftM (==0) (access nesting_lv) :: Eval Bool
initS = S M.empty M.empty M.empty 0 EmptyStack

local_vars = lens _local_vars $ \v s -> s{ _local_vars = v}
enclosing_vars = lens _enclosing_vars $ \v s -> s{ _enclosing_vars = v}
global_vars = lens _global_vars $ \v s -> s{ _global_vars = v}
control = lens _control $ \c s -> s{ _control = c}
nesting_lv = lens _nesting_lv $ \l s -> s{ _nesting_lv = l}

unwind :: (ControlStack -> Bool) -> Eval ControlStack
unwind pred = access control >>= go
  where
   go EmptyStack = error $ "unwindUptoWhile: empty control stack"
   go stack
     | pred stack = pop >> return stack
     | otherwise = pop >> unwind pred

unwindUptoWhile :: Eval ControlStack
unwindUptoWhile = access control >>= go
  where
   go EmptyStack = error $ "unwindUptoWhile: empty control stack"
   go ProcedureCall{} = error $ "unwindUptoWhile: procedureCall"
   go s@WhileLoop{} = return s

unwindPastWhile :: Eval ControlStack
unwindPastWhile = unwindUptoWhile >>= \s -> pop >> return s

push :: (ControlStack -> ControlStack) -> Eval ()
push frame = (control %= frame) >> return ()

pop :: Eval ()
pop = do
  stack <- access control
  case stack of
    EmptyStack -> error "pop: empty stack"
    _ -> control %= stack_tail >> return ()

isTrue :: Object -> Bool
isTrue (Bool b) = b
isTrue (Integer i) = i /= 0
isTrue _ = False

pass :: Eval Object
pass = return None

callFunction :: Procedure -> [Object] -> Eval Object
callFunction fun args = callCC $ \ret -> push (ProcedureCall ret) >> fun args

dumpStack :: ControlStack -> IO ()
dumpStack s = putStrLn "---top---" >> go s >> putStrLn "---bottom---"
  where
    go EmptyStack = putStrLn "EmptyStack"
    go WhileLoop{..} = putStrLn "WhileLoop" >> go stack_tail
    go ProcedureCall{..} = putStrLn "ProcedureCall" >> go stack_tail
