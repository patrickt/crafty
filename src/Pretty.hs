{-# LANGUAGE DerivingStrategies #-}
module Pretty
  ( module Prettyprinter
  , Prec (..)
  , prec
  , atom
  , withPrec
  )
  where

import Prettyprinter

-- Snatched from the Semantic definition. Correctly pretty-printing with precedence
-- is surprisingly, and sometimes depressingly, difficult.
data Prec a = Prec
  { level :: Maybe Int,
    body :: a
  }
  deriving stock (Eq, Ord, Show)

prec :: Int -> a -> Prec a
prec = Prec . Just

atom :: a -> Prec a
atom = Prec Nothing

withPrec :: Int -> Prec (Doc a) -> Doc a
withPrec lev (Prec mCurr bod)
  | maybe False (lev <) mCurr = parens bod
  | otherwise = bod
