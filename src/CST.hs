{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CST
  ( Ident (..),
    Primary (..),
    Expr (..),
    ExprF (..),
    Infix (..),
    Prefix (..),
    Stmt (..),
    Func (..),
    Decl (..),
  )
where

import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Foldable.TH
import Data.Scientific (Scientific)
import Data.String (IsString)
import Data.Text (Text)
import Prettyprinter (Pretty (..), (<+>))
import Prettyprinter qualified as Doc
import Prelude hiding (Ordering (..))

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
  pretty = cata \case
    AssignF a b -> a <+> "=" <+> b
    InfixF op a b -> a <+> pretty op <+> b
    DotF l r -> l <> "." <> pretty r
    PrefixF op a -> pretty op <> a
    CallF fn args -> fn <> Doc.tupled args
    PrimaryF p -> pretty p

data Func = Func Ident [Ident] [Decl]

instance Pretty Func where
  pretty (Func n args b) = pretty n <> Doc.tupled (fmap pretty args) <+> Doc.encloseSep "{" "}" Doc.hardline (fmap pretty b)


data Decl
  = Class Ident (Maybe Ident) [Func]
  | Fun Func
  | Var Ident (Maybe Expr)
  | Stmt Stmt

instance Pretty Decl where
  pretty = \case
    Class id ext fns ->
      Doc.hsep
        [ "class",
          pretty id,
          maybe mempty (\a -> "<" <+> pretty a) ext,
          Doc.encloseSep "{" "}" Doc.hardline (fmap pretty fns)
        ]
    Fun f -> "fun" <+> pretty f
    Var i e -> "var" <+> pretty i <+> maybe mempty (\a -> "=" <+> pretty a) e
    Stmt s -> pretty s

data Stmt
  = Expr Expr
  | For Stmt (Maybe Expr) (Maybe Expr) Stmt
  | If Expr Stmt (Maybe Stmt)
  | Print Expr
  | Return Expr
  | While Expr Stmt
  | Block [Decl]

makeBaseFunctor ''Stmt

instance Pretty Stmt where
  pretty = cata \case
    ExprF e -> pretty e
    ForF a b c s ->
      "for" <+> Doc.encloseSep "(" ")" ";" [a, maybe mempty pretty b, maybe mempty pretty c] <+> s
    --IfF a b c -> "if" <+> Doc.parens (pretty a) <+> b <> maybe ("else" <+>) mempty c
    PrintF e -> "print" <+> pretty e
    ReturnF e -> "return" <+> pretty e
    WhileF e s -> "while" <+> Doc.parens (pretty e) <+> s
    BlockF d -> Doc.encloseSep "{" "}" Doc.hardline (fmap pretty d)
