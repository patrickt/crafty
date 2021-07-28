{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CST
  ( Ident (..)
  , Primary (..)
  , Expr (..)
  , Infix (..)
  , Prefix (..)
  ) where

import Prelude hiding (Ordering (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Foldable.TH
import Data.String (IsString)
import Prettyprinter (Pretty (..), (<+>))
import Prettyprinter qualified as Doc

newtype Ident = Id Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Pretty)

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

data Primary
  = Bool Bool
  | Nil
  | This
  | Number (Either Integer Scientific)
  | Ident Ident
  | Paren Expr
  | Super Ident
  deriving stock (Show, Eq)

instance Pretty Primary where
  pretty = \case
    Bool True -> "true"
    Bool False -> "false"
    Nil -> "nil"
    This -> "this"
    Number n -> either pretty Doc.viaShow n
    Ident i -> pretty i
    Paren e -> Doc.parens (pretty e)
    Super i -> "super." <> pretty i

data Expr
  = Assign Expr Expr
  | Infix Infix Expr Expr
  | Dot Expr Ident
  | Prefix Prefix Expr
  | Call Expr [Expr]
  | Primary Primary
  deriving stock (Show, Eq)

makeBaseFunctor ''Expr

instance Pretty Expr where
  pretty = cata $ \case
    AssignF a b -> a <+> "=" <+> b
    InfixF op a b -> a <+> pretty op <+> b
    DotF l r -> l <> "." <> pretty r
    PrefixF op a -> pretty op <> a
    CallF fn args -> fn <> Doc.tupled args
    PrimaryF p -> pretty p
