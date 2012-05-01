{
module Hython.Parser.Parser where

import Control.Monad.State.Strict
import Data.Maybe (isJust)
import Data.List (foldl')
import Data.Either (rights)
import Hython.Parser.Token
import Hython.Parser.ParserMonad
import Hython.Parser.Location
import Hython.Parser.Lexer
import Hython.Parser.Error
import Hython.Parser.ADT
}

%name parseFile file
%tokentype { Token }
%monad { T } { (>>=) } { return }
%error { parseError }
%lexer { lexCont } { EOFToken {} }

%token
   '='             { AssignToken {} }
   '('             { LeftParenToken {} }
   ')'             { RightParenToken {} }
   ','             { CommaToken {} }
   ';'             { SemiColonToken {} }
   ':'             { ColonToken {} }
   '+'             { PlusToken {} }
   '-'             { MinusToken {} }
   '*'             { MultToken {} }
   '/'             { DivToken {} }
   '%'             { ModToken {} }
   '=='            { EqToken {} }
   '!='            { NotEqToken {} }
   '<'             { LessToken {} }
   '<='             { LessEqToken {} }
   '>'             { GreaterToken {} }
   '>='             { GreaterEqToken {} }
   'and'           { AndToken {} }
   'dedent'        { DedentToken {} }
   'elif'          { ElifToken {} }
   'else'          { ElseToken {} }
   'ident'         { IdentifierToken {} }
   'if'            { IfToken {} }
   'indent'        { IndentToken {} }
   'integer'       { IntToken {} }
   'newline'       { NewlineToken {} }
   'not'           { NotToken {} }
   'or'            { OrToken {} }
   'print'         { PrintToken {} }
   'while'         { WhileToken {} }
   'continue'         { ContinueToken {} }
   'break'         { BreakToken {} }

%%

pair(p,q) : p q { ($1, $2) }
left(p,q): p q { $1 }
right(p,q): p q { $2 }

or(p,q)
  : p { $1 }
  | q { $1 }

revlist1(p)
  : p             { [$1] }
  | revlist1(p) p { $2 : $1 }

many1(p) : revlist1(p) { reverse $1 }

many0(p)
  : many1(p) { $1 }
  |          { [] }

either(p,q)
  : p { Left $1 }
  | q { Right $1 }

opt(p)
  :   { Nothing }
  | p { Just $1 }

identifier :: { IdentS }
  : 'ident' { Ident (getSpan $1) (token_literal $1) }

atom :: { ExprS }
atom
  : identifier { Var (getSpan $1) $1 }
  | 'integer' { Int (getSpan $1) (token_int $1) }

file :: { [StatementS] }
file
  : many0(either('newline',statement)) { concat (rights $1) }


test :: { ExprS }
test : or_test { $1 }

or_op :: { OpS }
or_op : 'or' { Or (getSpan $1) }

or_test :: { ExprS }
or_test : and_test many0(pair(or_op,and_test)) { makeBinOp $1 $2 }

and_op :: { OpS }
and_op : 'and' { And (getSpan $1) }

and_test :: { ExprS }
and_test : not_test many0(pair(and_op,not_test)) { makeBinOp $1 $2 }

not_test :: { ExprS }
not_test
  : 'not' not_test { UnaryOp (spanning $1 $2) (Not (getSpan $1)) $2 }
  | comparison     { $1}

comparison :: { ExprS }
comparison : expr many0(pair(comp_op,expr)) { makeBinOp $1 $2 }

comp_op :: { OpS }
comp_op
  : '==' { Equal (getSpan $1) }
  | '!=' { NotEq (getSpan $1) }
  | '<' { Less (getSpan $1) }
  | '<=' { LessEq (getSpan $1) }
  | '>' { Greater (getSpan $1) }
  | '>=' { GreaterEq (getSpan $1) }
  -- TODO: in  not in  is  is not

plus_minus_op :: { OpS }
plus_minus_op
  : '+' { Plus (getSpan $1) }
  | '-' { Plus (getSpan $1) }

expr :: { ExprS }
expr : term many0(pair(plus_minus_op,term)) { makeBinOp $1 $2 }

mult_div_mod_op :: { OpS }
mult_div_mod_op
  : '*' { Multiply (getSpan $1) }
  | '/' { Divide (getSpan $1) }
  | '%' { Modulo (getSpan $1) }

term :: { ExprS }
term : factor many0(pair(mult_div_mod_op,factor)) { makeBinOp $1 $2 }

factor :: { ExprS }
factor
  : plus_minus_op factor { UnaryOp (spanning $1 $2) $1 $2 }
  | atom { $1 }



statement :: { [StatementS] }
statement
  : stmt_list { $1 }
  | compound_stmt { [$1] }

compound_stmt :: { StatementS }
compound_stmt
  : 'if' test ':' suite many0(elif) opt_else
    { Conditional (spanning (spanning (spanning $1 $4) $5) $6) (($2,$4):$5) $6 }
  | 'while' test ':' suite opt_else
    { While (spanning (spanning $1 $4) $5) $2 $4 $5 }
  -- TODO: for try ...

opt_else :: { [StatementS] }
opt_else
  :                  { [] }
  | 'else' ':' suite { $3 }

elif :: { (ExprS, [StatementS]) }
elif : 'elif' test ':' suite { ($2, $4) }

suite :: { [StatementS] }
suite
  : stmt_list { $1 }
  | {- no newline -} 'indent' many1(statement) 'dedent' { concat $2 }

stmt_list :: { [StatementS] }
stmt_list
  : small_stmts opt(';') 'newline' { reverse $1 }

small_stmts :: { [StatementS] }
small_stmts
  : small_stmt                 { [$1] }
  | small_stmts ';' small_stmt { $3 : $1 }

small_stmt :: { StatementS }
small_stmt
  : test many_assign_stmt { makeAssignOrExpr $1 $2 }
  | 'continue' { Continue (getSpan $1) }
  | 'break' { Break (getSpan $1) }
  | 'print' test opt(',') { Print (spanning (spanning $1 $2) $3) $2 (isJust $3) }

many_assign_stmt :: { [ExprS] }
many_assign_stmt : many0(right('=',test)) { $1 }

{
makeBinOp :: ExprS -> [(OpS, ExprS)] -> ExprS
makeBinOp e es = foldl' (\e1 (op,e2) -> BinaryOp (spanning e1 e2) op e1 e2) e es

makeAssignOrExpr :: ExprS -> [ExprS] -> StatementS
makeAssignOrExpr e [] = StmtExpr (getSpan e) e
makeAssignOrExpr e es@(_:_) = Assign (spanning e es) (e : init es) (last es)

parser :: String -> Either ParseError [StatementS]
parser code = parseCode code parseFile
}
