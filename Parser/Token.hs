module Hython.Parser.Token (
  Token(..)
  ) where

import Hython.Parser.Location

data Token =
  IndentToken { token_span :: !Span }
  | DedentToken { token_span :: !Span }
  | NewlineToken { token_span :: !Span }

  -- Identifiers
  | IdentifierToken { token_span :: !Span, token_literal :: !String }

  -- Literals
  | IntToken { token_span :: !Span, token_int :: !Integer }
  | StringToken { token_span :: !Span, token_string :: !String }

  -- Keywords
  | IfToken { token_span :: !Span }
  | ElifToken { token_span :: !Span }
  | WhileToken { token_span :: !Span }
  | NotToken { token_span :: !Span }
  | NoneToken { token_span :: !Span }
  | TrueToken { token_span :: !Span }
  | FalseToken { token_span :: !Span }
  | ElseToken { token_span :: !Span }

  -- Operators
  | PlusToken { token_span :: !Span }
  | MinusToken { token_span :: !Span }
  | TildeToken { token_span :: !Span }
  | MultToken { token_span :: !Span }
  | DivToken { token_span :: !Span }
  | ModToken { token_span :: !Span }
  | GreaterToken { token_span :: !Span }
  | LessToken { token_span :: !Span }
  | GreaterEqToken { token_span :: !Span }
  | LessEqToken { token_span :: !Span }
  | EqToken { token_span :: !Span }
  | NotEqToken { token_span :: !Span }
  | AndToken { token_span :: !Span }
  | OrToken { token_span :: !Span }
  | BreakToken { token_span :: !Span }
  | ContinueToken { token_span :: !Span }
  | AssignToken { token_span :: !Span }
  | ReturnToken { token_span :: !Span }
  | PassToken { token_span :: !Span }

  -- Delimiters
  | DefToken { token_span :: !Span }
  | LambdaToken { token_span :: !Span }
  | LeftParenToken { token_span :: !Span }
  | RightParenToken { token_span :: !Span }
  | LeftBracketToken { token_span :: !Span }
  | RightBracketToken { token_span :: !Span }
  | LeftBraceToken { token_span :: !Span }
  | RightBraceToken { token_span :: !Span }
  | ColonToken { token_span :: !Span }
  | SemiColonToken { token_span :: !Span }
  | CommaToken { token_span :: !Span }

  -- Special
  | PrintToken { token_span :: !Span }
  | EOFToken { token_span :: !Span }
  deriving (Show)

instance SpanClass Token where
  getSpan = token_span
