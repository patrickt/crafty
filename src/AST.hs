{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AST (AST (..), Syn, Fix (..), convert, truthy) where

import CST qualified
import Data.Fix (Fix (..))
import Data.Functor.Classes
import Data.Functor.Const
import Data.Functor.Foldable hiding (Nil)
import Data.Scientific
import GHC.Generics (Generic, Generic1)
import Generic.Data qualified as Data
import Env (Env)
import Generic.Data.Orphans ()
import Ident

data AST a
  = Bool Bool
  | Nil
  | This
  | Number (Either Integer Scientific)
  | Ident Ident
  | Super a
  | Infix CST.Infix a a
  | Dot a a
  | Prefix CST.Prefix a
  | Call a [a]
  | Assign a a
  | Func [a] a
  | Class a (Maybe a) [a]
  | Var a (Maybe a)
  | For a (Maybe a) (Maybe a) a
  | If a a (Maybe a)
  | Print a
  | Return a
  | While a a
  | Scope a
  | !a :. a
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via Data.Generically1 AST

truthy :: AST a -> Bool
truthy (Bool b) = b
truthy Nil = False
truthy _ = True

type instance Base (AST a) = Const (AST a)

instance Recursive (AST a) where project = Const

instance Corecursive (AST a) where embed = getConst

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
      CST.Number n -> Number n
      CST.Ident i -> Ident i
      CST.Super a -> Super (convert a)

instance Convert CST.Expr where
  convert = cata \case
    CST.AssignF a b -> Fix (Assign a b)
    CST.InfixF op a b -> Fix (Infix op a b)
    CST.DotF l r -> Fix (Dot l (convert r))
    CST.PrefixF op a -> Fix (Prefix op a)
    CST.CallF fn args -> Fix (Call fn args)
    CST.PrimaryF p -> convert p

instance Convert CST.Func where
  convert (CST.Func n args bod) = Fix (Assign (Fix (Ident n)) (Fix (Func (convert <$> args) (convert bod))))

instance Convert CST.Decl where
  convert = \case
    CST.Class a b c -> Fix (Class (convert a) (convert <$> b) (convert <$> c))
    CST.Fun f -> convert f
    CST.Var i e -> Fix (Var (convert i) (convert <$> e))
    CST.Stmt s -> convert s

instance Convert [CST.Decl] where
  convert cs = Fix (Scope (foldr (\x y -> Fix (convert x :. y)) (Fix Nil) cs))

instance Convert CST.Stmt where
  convert = cata \case
    CST.ExprF e -> convert e
    CST.ForF a b c d -> Fix (For a (fmap convert b) (convert <$> c) d)
    CST.IfF a b c -> Fix (If (convert a) b c)
    CST.PrintF a -> Fix (Print (convert a))
    CST.ReturnF a -> Fix (Return (convert a))
    CST.WhileF a b -> Fix (While (convert a) b)
    CST.BlockF a -> Fix (Scope (foldr (\x y -> Fix (x :. y)) (Fix Nil) (convert <$> a)))
