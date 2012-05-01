{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
module Hython.Core.Prim (
  ControlStack(..), Eval, push, pop, initS,
  Object(..), vars, control, pass,
  isTrue, unwindPastWhile, unwindUptoWhile
  ) where

import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.Error
import Data.Lens.Strict
import Data.Lens.Template
import qualified Hython.Parser.ADT as ADT
import Hython.Parser.Location
import qualified Data.Map as M

data Object = None | Integer Integer | Bool Bool
  deriving (Show)

data ControlStack =
  EmptyStack
  | WhileLoop
    { loop_start :: Eval Object
    , loop_end :: Eval Object
    , stack_tail :: ControlStack
    }

data S = S { _vars :: M.Map (ADT.Ident Span) Object, _control :: ControlStack }

type Eval a = StateT S (ContT Object IO) a

initS = S M.empty EmptyStack

$(makeLenses [''S])

unwindUptoWhile :: Eval ControlStack
unwindUptoWhile = access control >>= go
  where
   go EmptyStack = error $ "unwindUptoWhile: empty control stack"
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

break :: Eval Object
break = unwindPastWhile >>= loop_end

continue :: Eval Object
continue = unwindUptoWhile >>= loop_start
