{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AST (AST (..), Syn, Fix (..), convert) where

import CST qualified
import Data.Fix (Fix (..))
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import Data.Scientific
import GHC.Generics (Generic, Generic1)
import Generic.Data qualified as Data
import Generic.Data.Orphans ()
import Ident

data AST a
  = Bool Bool
  | Nil
  | This
  | Number Scientific
  | Ident Ident
  | Paren a
  | Super a
  | Infix CST.Infix a a
  | Dot a a
  | Prefix CST.Prefix a
  | Call a [a]
  | Assign a a
  | Func a [a] [a]
  | Class a (Maybe a) [a]
  | Var a (Maybe a)
  | For a (Maybe a) (Maybe a) a
  | If a a (Maybe a)
  | Print a
  | Return a
  | While a a
  | Block [a]
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via Data.Generically1 AST

instance Show a => Show (AST a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq a => Eq (AST a) where
  (==) = liftEq (==)

type Syn = Fix AST

class Convert a where
  convert :: a -> Syn

instance Convert Ident where
  convert = Fix . Ident

instance Convert CST.Primary where
  convert =
    Fix . \case
      CST.Bool b -> Bool b
      CST.Nil -> Nil
      CST.This -> This
      CST.Number n -> Number (either fromIntegral id n)
      CST.Ident i -> Ident i
      CST.Paren a -> Paren (convert a)
      CST.Super a -> Super (convert a)

instance Convert CST.Expr where
  convert x = cata go x
    where
      go = \case
        CST.AssignF a b -> Fix (Assign a b)
        CST.InfixF op a b -> Fix (Infix op a b)
        CST.DotF l r -> Fix (Dot l (convert r))
        CST.PrefixF op a -> Fix (Prefix op a)
        CST.CallF fn args -> Fix (Call fn args)
        CST.PrimaryF p -> convert p

instance Convert CST.Func where
  convert (CST.Func n args bod) = Fix (Func (convert n) (convert <$> args) (convert <$> bod))

instance Convert CST.Decl where
  convert = \case
    CST.Class a b c -> Fix (Class (convert a) (convert <$> b) (convert <$> c))
    CST.Fun f -> convert f
    CST.Var i e -> Fix (Var (convert i) (convert <$> e))
    CST.Stmt s -> convert s

instance Convert CST.Stmt where
  convert = \case
    CST.Expr e -> convert e
    CST.For a b c d -> Fix (For (convert a) (convert <$> b) (convert <$> c) (convert d))
    CST.If a b c -> Fix (If (convert a) (convert b) (convert <$> c))
    CST.Print a -> Fix (Print (convert a))
    CST.Return a -> Fix (Return (convert a))
    CST.While a b -> Fix (While (convert a) (convert b))
    CST.Block a -> Fix (Block (convert <$> a))
