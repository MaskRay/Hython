{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, RecordWildCards #-}
module Hython.Parser.Utils (
  Trailer(..), TrailerS, makeBinOp, makeAssignOrExpr, makeTrailer
  ) where

import Data.List
import Hython.Parser.Error
import Hython.Parser.Location
import Hython.Parser.ADT

data Trailer a
   = TrailerCall { trailer_annot :: a, trailer_call_args :: [Expr a] }
type TrailerS = Trailer Span
instance SpanClass TrailerS where getSpan = trailer_annot

makeBinOp :: ExprS -> [(OpS, ExprS)] -> ExprS
makeBinOp e es = foldl' (\e1 (op,e2) -> BinaryOp (spanning e1 e2) op e1 e2) e es

makeAssignOrExpr :: ExprS -> [ExprS] -> StatementS
makeAssignOrExpr e [] = StmtExpr (getSpan e) e
makeAssignOrExpr e es@(_:_) = Assign (spanning e es) (e : init es) (last es)

makeTrailer :: ExprS -> [TrailerS] -> ExprS
makeTrailer = foldl' go
  where
    go e t@TrailerCall{..} = Call (spanning e t) e trailer_call_args
