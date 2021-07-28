{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module Op (module Op, Assoc (..)) where

import Prelude hiding (Ordering (..))
import Prettyprinter (Pretty (..))
import Text.Parser.Expression (Assoc (..))

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

precedence :: Infix -> Int
precedence = \case
  Or -> 12
  And -> 11
  Eq -> 7
  Neq -> 7
  Plus -> 4
  Minus -> 4
  Mult -> 3
  Div -> 3
  _ -> 6

associativity :: Infix -> Assoc
associativity = \case
  Or -> AssocRight
  LTE -> AssocNone
  LT -> AssocNone
  GT -> AssocNone
  GTE -> AssocNone
  _ -> AssocLeft

data Prefix = Not | Neg
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty Prefix where
  pretty = \case
    Not -> "!"
    Neg -> "-"
