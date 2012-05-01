module Hython.Parser.Error (
  ParseError(..)
  ) where

import Control.Monad.Error.Class
import Hython.Parser.Location
import Hython.Parser.Token

data ParseError =
  UnexpectedToken Token
  | UnexpectedChar Char Location
  | StrError String
  deriving (Show)

instance Error ParseError where
  noMsg = StrError ""
  strMsg = StrError
