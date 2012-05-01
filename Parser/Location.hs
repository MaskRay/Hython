{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Hython.Parser.Location (
  Location(..),
  Span(..),
  SpanClass(..),
  spanning
) where

import Data.List (foldl1')

data Location = Location
  { loc_row :: !Int
  , loc_col :: !Int
  } deriving (Show)

data Span = SpanEmpty
  | SpanPoint { span_row :: !Int, span_col :: !Int }
  | SpanRange { span_row1 :: !Int, span_col1 :: !Int, span_row2 :: !Int, span_col2 :: !Int }

instance Show Span where
  showsPrec _ SpanEmpty = id
  showsPrec _ SpanPoint{..} = shows (span_row, span_col)
  showsPrec p SpanRange{..} = showParen (p > 6) $ shows (span_row1,span_col1) . ('-':) . shows (span_row2,span_col2)

class SpanClass a where
  getSpan :: a -> Span

instance SpanClass a => SpanClass (Maybe a) where
  getSpan Nothing = SpanEmpty
  getSpan (Just x) = getSpan x

instance (SpanClass a, SpanClass b) => SpanClass (Either a b) where
   getSpan (Left x) = getSpan x
   getSpan (Right x) = getSpan x

instance (SpanClass a, SpanClass b) => SpanClass (a, b) where
   getSpan (x,y) = spanning x y

instance SpanClass Span where
  getSpan = id

instance SpanClass Location where
  getSpan Location{..} = SpanPoint
    { span_row = loc_row
    , span_col = loc_col
    }

instance SpanClass a => SpanClass [a] where
  getSpan [] = SpanEmpty
  getSpan [x] = getSpan x
  getSpan xs = combineSpan (getSpan (head xs)) (getSpan (last xs))


combineSpan :: Span -> Span -> Span
combineSpan SpanEmpty x = x
combineSpan x SpanEmpty = x
combineSpan start@SpanRange{span_row1=row1, span_col1=col1} end@SpanRange{span_row2=row2,span_col2=col2} = case row1 `compare` row2 of
  EQ -> case col1 `compare` col2 of
    EQ -> SpanPoint row1 col1
    LT -> SpanRange row1 col1 row2 col2
  LT -> SpanRange row1 col1 row2 col2

spanning :: (SpanClass a, SpanClass b) => a -> b -> Span
spanning x y = combineSpan (getSpan x) (getSpan y)
