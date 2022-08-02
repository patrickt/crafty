{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Env
  ( Env,
    empty,
    lookup,
    insert,
    fromList,
    push,
    pop
  )
where

import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HM
import Data.Text.Prettyprint.Doc qualified as Pretty
import Ident
import Prettyprinter (Pretty (..))
import Prelude hiding (lookup)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Generic.Data qualified as Data
import Generic.Data.Orphans ()
import GHC.Generics (Generic1)

newtype Env a = Env [HashMap Ident a]
  deriving stock (Functor, Show, Foldable, Traversable, Generic1)
  deriving newtype (Semigroup)
  deriving (Show1) via Data.Generically1 Env

instance Eq1 Env where
  liftEq fn (Env a) (Env b) = liftEq (liftEq fn) a b

instance Eq a => Eq (Env a) where (==) = liftEq (==)

instance Pretty a => Pretty (Env a) where
  pretty (Env e) = do
    let pair (a, b) = pretty a <> "=" <> pretty b
    let elements = fmap pair (HM.toList (fold e))
    Pretty.list elements

empty :: Env a
empty = Env (pure HM.empty)

fromList :: Foldable f => f (Ident, a) -> Env a
fromList = Env . pure . HM.fromList . toList

lookup :: Ident -> Env a -> Maybe a
lookup _ (Env []) = Nothing
lookup n (Env (a:rest)) = case HM.lookup n a of
  Just it -> pure it
  Nothing -> lookup n (Env rest)

insert :: Ident -> a -> Env a -> Env a
insert n v (Env []) = Env [HM.singleton n v]
insert n v (Env (x:rest)) = Env (HM.insert n v x : rest)

push :: Env a -> Env a
push (Env x) = Env (mempty : x)

pop :: Env a -> Env a
pop (Env x) = Env (drop 1 x)
