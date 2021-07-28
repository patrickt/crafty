{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module Op (module Op) where

import Prelude hiding (Ordering (..))
import Prettyprinter (Pretty (..))

data Infix = Or | And | Eq | Neq | Plus | Minus | Mult | Div | LT | LTE | GT | GTE
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty Infix where
  pretty = \case
    Or -> "or"
    And -> "and"
    Eq -> "=="
    Neq -> "!="
    Plus -> "+"
    Minus -> "-"
    Mult -> "*"
    Div -> "/"
    LT -> "<"
    LTE -> "<="
    GT -> ">"
    GTE -> ">="

data Prefix = Not | Neg
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty Prefix where
  pretty = \case
    Not -> "!"
    Neg -> "-"
