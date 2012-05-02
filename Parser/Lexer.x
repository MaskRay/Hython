{
{-# LANGUAGE NamedFieldPuns #-}
module Hython.Parser.Lexer (
  lexer, T, lexCont, parseCode
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.Char
import Data.Lens.Strict
import Numeric
import qualified Data.Map as Map
import Hython.Parser.Location
import Hython.Parser.Token
import Hython.Parser.ParserMonad
import Hython.Parser.Error
}

%wrapper "posn"

$ident_letter = [a-zA-Z_]
$digit = 0-9
@eol = \r \n | \r | \n
$non_eol = ~ [\n \r]

:-

[\ \f\t\v]+ ;
\# ($non_eol)* ;

<bof> {
  @eol { endOfLine }
  () { indent BOF dedentation }
}

<bol> {
  @eol { endOfLine }
  () { indent BOL dedentation }
}

<dedentation> () { dedent }

<0> {
  @eol { \p s -> pushStartCode bol >> endOfLine p s }

  [1-9] $digit* { int (fst.head.readDec) }
  0 { int (const 0) }
  0 [0-7]+ { int (fst.head.readOct.tail) }
  0 (o|O) [0-7]+ { int (fst.head.readOct.drop 2) }
  0 (x|X) [0-9a-fA-F]+ { int (fst.head.readHex.drop 2) }

  $ident_letter ($ident_letter|$digit)* { identOrKeyword }

  "(" { leftParen LeftParenToken }
  ")" { rightParen RightParenToken }
  "+" { symbol PlusToken }
  "-" { symbol MinusToken }
  "*" { symbol MultToken }
  "/" { symbol DivToken }
  "=" { symbol AssignToken }
  ":" { symbol ColonToken }
  "==" { symbol EqToken }
  "!=" { symbol NotEqToken }
  "<" { symbol LessToken }
  "<=" { symbol LessEqToken }
  ">" { symbol GreaterToken }
  ">=" { symbol GreaterEqToken }
}

{

type Action = AlexPosn -> String -> P AlexInput Token

symbol :: (Span -> Token) -> Action
symbol mk pos _ = return (mk (getSpan pos))

int :: (String -> Integer) -> Action
int mk pos lit = return (IntToken (getSpan pos) (mk lit))

pos2loc :: AlexPosn -> Location
pos2loc (AlexPn _ r c) = Location r (c-1)

instance SpanClass AlexPosn where
  getSpan (AlexPn _ r c) = SpanPoint r (c-1)

leftParen :: (Span -> Token) -> Action
leftParen mk pos s = do
  let t = mk (getSpan pos)
  pushParen t
  return t

rightParen :: (Span -> Token) -> Action
rightParen mk pos s = do
  let t = mk (getSpan pos)
      match (LeftParenToken _) (RightParenToken _) = True
      match (LeftBracketToken _) (RightBracketToken _) = True
      match (LeftBraceToken _) (RightBraceToken _) = True
      match _ _ = False
  top <- getParen
  if match top t
     then popParen >> return t
     else throwError $ StrError "parentheses/brackets/braces mismatch"

identOrKeyword :: Action
identOrKeyword pos s = do
  let ident = takeWhile (liftM2 (||) isAlphaNum (=='/')) s
  case Map.lookup ident keywords of
    Nothing -> return $ IdentifierToken (getSpan pos) ident
    Just mk -> symbol mk pos ident

keywords :: Map.Map String (Span -> Token)
keywords = Map.fromList
   [ ("and", AndToken), ("break", BreakToken)
   , ("continue", ContinueToken)
   , ("or", OrToken)
   , ("not", NotToken)
   , ("else", ElseToken)
   , ("if", IfToken)
   , ("elif", ElifToken)
   , ("print", PrintToken)
   , ("while", WhileToken)
   , ("return", ReturnToken)
   , ("def", DefToken)
   ]

data BO = BOF | BOL

newline :: P AlexInput Token
newline = liftM (NewlineToken . getSpan) getLastEOL

endOfLine :: Action
endOfLine _ _ = do
  loc <- getLocation
  setLastEOL loc
  lexToken

dedent :: Action
dedent _ _ = do
  loc <- getLocation
  ((AlexPn _ _ c),_,_,_) <- getInput
  indent:_ <- getIndentStack
  case compare indent (c-1) of
    EQ -> popStartCode >> lexToken
    LT -> throwError $ StrError "indentation error"
    GT -> popIndent >> return (DedentToken (getSpan loc))

indent :: BO -> Int -> Action
indent bo dedentCode pos "" = do
  popStartCode
  case bo of
    BOF -> lexToken
    BOL -> newline
indent bo dedentCode pos str = do
  popStartCode
  paren <- getParenStack
  loc <- getLocation
  ((AlexPn _ _ c),_,_,_) <- getInput
  if not $ null paren
     then lexToken
     else do
       indent:_ <- getIndentStack
       case compare indent (c-1) of
         EQ -> case bo of
           BOF -> lexToken
           BOL -> newline
         LT -> pushIndent (c-1) >> return (IndentToken (getSpan loc))
         GT -> pushStartCode dedentCode >> newline

lexToken :: P AlexInput Token
lexToken = do
  input@(AlexPn _ r c,_,_,str) <- getInput
  loc <- getLocation
  sc <- getStartCode
  case alexScan input sc of
    AlexEOF -> do
       stack <- getIndentStack
       if length stack == 1
          then return $ EOFToken (getSpan loc)
          else popIndent >> return (DedentToken (getSpan loc))
    AlexError _ -> throwError . StrError $ "row " ++ show r ++ " col " ++ show (c-1) ++ " error"
    AlexSkip rest@(pos,_,_,_) len -> do
       setLocation (pos2loc pos)
       setInput rest
       lexToken
    AlexToken rest@(pos@(AlexPn _ r' c'),_,_,_) len action -> do
      setLocation (pos2loc pos)
      setInput rest
      token <- action pos str
      return token { token_span = SpanRange { span_row1 = r, span_col1 = c-1, span_row2 = r', span_col2 = c'-1 } }

lexCont :: (Token -> T a) -> T a
lexCont cont = lexToken >>= cont

lexer :: String -> Either ParseError [Token]
lexer code = evalStateT (setInput (alexStartPos,'\n',[],code) >> loop []) initState'
  where
    loop ts = do
      t <- lexToken
      case t of
         EOFToken {} -> return (reverse ts)
         _ -> loop (t:ts)

type T a = P AlexInput a

parseCode code cont = evalStateT (setInput (alexStartPos,'\n',[],code) >> cont) initState'

initState' = initState bof

}
