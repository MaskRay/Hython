{-# LANGUAGE TemplateHaskell #-}
module Hython.Parser.ParserMonad (
  P, ParseState, initState, parseError,
  getInput, setInput,
  getLastEOL, setLastEOL,
  getLocation, setLocation,
  getStartCode, pushStartCode, popStartCode,
  getIndentStack, pushIndent, popIndent,
  getParen, getParenStack, pushParen, popParen,
  ) where

import Control.Monad.Error
import Data.Word (Word8)
import Control.Monad.State.Strict
import Data.Lens.Common
import Data.Lens.Strict
import Data.Lens.Template
import Hython.Parser.Location
import Hython.Parser.Error
import Hython.Parser.Token

data ParseState i = ParseState
  { _location :: !Location
  , _lastEOL :: Location
  , _input :: i
  , _startCodeStack :: [Int]
  , _indentStack :: [Int]
  , _parenStack :: [Token]
  } deriving Show
$(makeLenses [''ParseState])

type P i a = StateT (ParseState i) (Either ParseError) a

initState :: Int -> ParseState i
initState bof = ParseState (Location 1 0) undefined undefined [bof,0] [0] []

mkGS :: Lens (ParseState i) a -> (P i a, a -> P i a)
mkGS lens = (access lens, (lens~=))

(getInput, setInput) = mkGS input
(getLastEOL, setLastEOL) = mkGS lastEOL
(getLocation, setLocation) = mkGS location

getIndentStack :: P i [Int]
getIndentStack = access indentStack

getParen :: P i Token
getParen = do
  stack <- getParenStack
  case stack of
    [] -> throwError $ StrError "parenthesis mismatch"
    x:_ -> return x

getStartCode :: P i Int
getStartCode = do
  stack <- access startCodeStack
  case stack of
    [] -> throwError $ StrError "parenthesis mismatch"
    x:_ -> return x

getParenStack = access parenStack

mkPushPop :: Lens (ParseState i) [a] -> (a -> P i [a], P i a)
mkPushPop lens = (\t -> lens %= (t:), do
  stack <- access lens
  case stack of
    [] -> throwError $ StrError "attempt to pop empty stack"
    x:xs -> lens ~= xs >> return x)

(pushStartCode, popStartCode) = mkPushPop startCodeStack
(pushIndent, popIndent) = mkPushPop indentStack
(pushParen, popParen) = mkPushPop parenStack

parseError :: Token -> P i a
parseError = throwError . UnexpectedToken
