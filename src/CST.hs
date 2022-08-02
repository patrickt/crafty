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
    StmtF (..),
    Func (..),
    Decl (..),
    module Op,
  )
where

import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Foldable.TH
import Data.Scientific (Scientific)
import Ident
import Op
import Pretty (Pretty (..), (<+>))
import Pretty qualified as Doc
import Prelude hiding (Ordering (..))

data Primary
  = Bool Bool
  | Nil
  | This
  | Number (Either Integer Scientific)
  | Ident Ident
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
  pretty = Doc.body . cata go
    where
      go = \case
        AssignF a b -> Doc.prec 14 (Doc.withPrec 14 a <+> "=" <+> Doc.withPrec 15 b)
        DotF a b -> Doc.prec 1 (Doc.withPrec 0 a <> "." <> pretty b)
        InfixF op a b -> let x = Op.precedence op - 1 in
          Doc.prec (x + 1) (Doc.withPrec x a <+> pretty op <+> Doc.withPrec x b)
        PrefixF op a -> Doc.prec 2 (pretty op <> Doc.withPrec 1 a)
        CallF fn args -> Doc.prec 1 (Doc.withPrec 1 fn <> Doc.tupled (fmap (Doc.withPrec 0) args))
        PrimaryF p -> Doc.atom (pretty p)

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
    Class ident ext fns ->
      Doc.hsep
        [ "class",
          pretty ident,
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
    IfF cond block alt -> "if" <+> Doc.parens (pretty cond) <+> block <+> maybe mempty ("else" <+>) alt
    BlockF d -> Doc.encloseSep "{" "}" Doc.hardline (fmap pretty d)
