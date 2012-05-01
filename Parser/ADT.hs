{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Hython.Parser.ADT where

import Hython.Parser.Location

class Annotated t where
  annot :: t a -> a

data Ident a = Ident { ident_annot :: a, ident_string :: !String }
  deriving (Show)
type IdentS = Ident Span
instance Eq IdentS where
  Ident _ x == Ident _ y = x == y
instance Ord IdentS where
  compare (Ident _ x) (Ident _ y) = compare x y
instance Annotated Ident where
  annot = ident_annot
instance SpanClass IdentS where
  getSpan = annot

data Statement a =
  Conditional { stmt_annot :: a
              , cond_guards :: [(Expr a, Suite a)]
              , cond_else :: Suite a
              }
  | While { stmt_annot :: a
          , while_cond :: Expr a
          , while_body :: Suite a
          , while_else :: Suite a
          }
  | Assign { stmt_annot :: a
           , assign_to :: [Expr a]
           , assign_expr :: Expr a
           }
  | Print { stmt_annot :: a
          , print_exprs :: Expr a
          , print_comma :: Bool
          } -- TODO: add chevron
  | StmtExpr { stmt_annot :: a, stmt_expr :: Expr a  }
  | Continue { stmt_annot :: a }
  | Break { stmt_annot :: a }
  deriving (Show)

type StatementS = Statement Span
instance Annotated Statement where
  annot = stmt_annot
instance SpanClass StatementS where
  getSpan = annot

type Suite a = [Statement a]

data Expr a =
  Var { expr_annot :: a, var_ident :: Ident a }
  | Int { expr_annot :: a, int_value :: Integer }
  | Bool { expr_annot :: a, bool_value :: Bool }
  | None { expr_annot :: a }
  | UnaryOp { expr_annot :: a, op :: Op a, operand :: Expr a }
  | BinaryOp { expr_annot :: a, op :: Op a, op_lhs :: Expr a, op_rhs :: Expr a }
  deriving (Show)

type ExprS = Expr Span
instance Annotated Expr where
  annot = expr_annot
instance SpanClass ExprS where
  getSpan = annot

data Op a =
  And { op_annot :: a }
  | Or { op_annot :: a }
  | Not { op_annot :: a }
  | Equal { op_annot :: a }
  | NotEq { op_annot :: a }
  | Less { op_annot :: a }
  | Greater { op_annot :: a }
  | LessEq { op_annot :: a }
  | GreaterEq { op_annot :: a }

  | Plus { op_annot :: a }
  | Minus { op_annot :: a }
  | Multiply { op_annot :: a }
  | Divide { op_annot :: a }
  | Modulo { op_annot :: a }
  deriving (Show)
type OpS = Op Span
instance Annotated Op where
  annot = op_annot
instance SpanClass OpS where
  getSpan = annot
