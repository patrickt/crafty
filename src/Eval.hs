{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Eval (eval) where

import AST (AST, Syn)
import AST qualified
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data qualified as Data
import Data.Function (fix)
import Data.Functor.Foldable hiding (Nil)
import Data.Generics.Sum.Typed
import Data.Scientific
import Env (Env)
import Env qualified
import Generic.Data.Orphans ()
import Heap (Addr, Heap)
import Heap qualified
import Ident
import Op qualified
import Optics
import Prelude hiding (lookup)
import Control.Monad

data EvalError
  = Unbound Ident
  | TypeError String
  | DanglingPointer Addr
  | Unimplemented String
  | Other String
  deriving stock (Eq, Show)

newtype EarlyReturn = EarlyReturn Val

class IsAST a where
  fixed :: Iso' a (AST Syn)

instance IsAST (AST Syn) where fixed = simple

instance IsAST Syn where fixed = coerced

-- demanding ::
--   (JoinKinds An_Iso l k, Is k An_AffineFold, IsAST s, Has (Throw EvalError) sig m) =>
--   Optic l is (AST Syn) (AST Syn) a a ->
--   String ->
--   s ->
--   m a
-- demanding p name ast = do
--   case ast ^? fixed % p of
--     Just a -> pure a
--     Nothing -> throwError (TypeError name (ast ^. fixed))

-- demand :: (IsAST s, AsType a (AST Syn), Has (Throw EvalError) sig m) => String -> s -> m a
-- demand = demanding _Typed

orThrow :: (Has (Throw e) sig m) => e -> Maybe a -> m a
orThrow e = maybe (throwError e) pure

data Val
  = Int Int
  | Float Scientific
  | Ref Addr
  | Bool Bool
  | Nil
  deriving stock (Eq, Show, Data.Data)

nature :: Val -> String
nature = Data.showConstr . Data.toConstr

applyNum :: (Has (Throw EvalError) sig m) => (forall a. Num a => a -> a -> a) -> Val -> Val -> m Val
applyNum fn a b = case (a, b) of
  (Int i, Int j) -> pure (Int (fn i j))
  (Float f, Float g) -> pure (Float (fn f g))
  _ -> throwError (TypeError ("expected " <> nature a <> " got " <> nature b))

truthy :: Val -> Bool
truthy = \case
  Bool False -> False
  Nil -> False
  _ -> True

applyBool :: Applicative m => (Bool -> Bool -> Bool) -> Val -> Val -> m Val
applyBool fn a b = pure (Bool (fn (truthy a) (truthy b)))

applyOrd :: (Has (Throw EvalError) sig m) => (forall a. (Ord a, Num a) => a -> a -> Bool) -> Val -> Val -> m Val
applyOrd fn a b =
  case (a, b) of
    (Int i, Int j) -> pure (Bool (fn i j))
    (Float f, Float g) -> pure (Bool (fn f g))
    _ -> throwError (TypeError ("expected " <> nature a <> " got " <> nature b))

fetch :: (Has (State (Heap Val)) sig m, Has (Throw EvalError) sig m) => Val -> m Val
fetch = \case
  Ref addr -> gets (Heap.lookup addr) >>= orThrow (DanglingPointer addr) >>= fetch
  other -> pure other

lookup ::
  ( Has (Reader (Env Addr)) sig m,
    Has (State (Heap Val)) sig m,
    Has (Throw EvalError) sig m
  ) =>
  Ident ->
  m Val
lookup i = do
  slot <- asks (Env.lookup i) >>= orThrow (Unbound i)
  gets (Heap.lookup slot) >>= orThrow (DanglingPointer slot)

eval ::
  forall m sig.
  ( Has (Throw EvalError) sig m,
    Has (Throw EarlyReturn) sig m,
    Has (Reader (Env Addr)) sig m,
    Has (State (Heap Val)) sig m,
    MonadIO m
  ) =>
  Syn ->
  m Val
eval = cataA go
  where
    go :: AST (m Val) -> m Val
    go x = case x of
      AST.Bool b -> pure (Bool b)
      AST.Nil -> pure Nil
      AST.This -> lookup "this"
      AST.Print p -> do
        p >>= fetch >>= liftIO . print
        pure Nil
      AST.Return t -> t >>= throwError . EarlyReturn
      AST.Infix op l r -> join (perform <$> l <*> r)
        where
          perform :: Val -> Val -> m Val
          perform = case op of
            Op.And -> applyBool (&&)
            Op.Or -> applyBool (||)
            Op.Eq -> applyOrd (==)
            Op.Neq -> applyOrd (/=)
            Op.Plus -> applyNum (+)
            Op.Minus -> applyNum (-)
            Op.Mult -> applyNum (*)
            Op.Div -> \_ _ -> throwError (Unimplemented "division")
            Op.LT -> applyOrd (<)
            Op.GT -> applyOrd (>)
            Op.LTE -> applyOrd (<=)
            Op.GTE -> applyOrd (>=)


-- pf = pure . Fix
-- go = \case
--   Bool b -> pf (Bool b)
--   Nil -> pf Nil
--   This -> pf This
--   Number n -> pf (Number n)
--   Ident i ->
--     asks (Env.lookup i)
--       >>= orThrow (Unbound i)
--       >>= \s -> gets (Heap.lookup s)
--       >>= orThrow (DanglingPointer i slot)
--   Infix op l r -> case op of
--     Op.Or -> needBool (||) l r
--     Op.And -> needBool (&&) l r
--     Op.Eq -> Fix . Bool <$> ((==) <$> l <*> r)
--     Op.Neq -> Fix . Bool <$> ((/=) <$> l <*> r)
--     Op.Plus -> needSci (+) Number l r
--     Op.Minus -> needSci (-) Number l r
--     Op.Mult -> needSci (*) Number l r
--     Op.Div -> needSci (/) Number l r
--     Op.LT -> needSci (<) Bool l r
--     Op.LTE -> needSci (<=) Bool l r
--     Op.GT -> needSci (>) Bool l r
--     Op.GTE -> needSci (>=) Bool l r
--   a :. rest ->
--     a >>= \case
--       Fix (Assign (Fix (Ident lhs)) rhs) -> do
--         slot <- state (Heap.alloc rhs)
--         local @(Env Addr) (Env.insert lhs slot) rest
--       Fix (Var (Fix (Ident name)) mRhs) -> do
--         rhs <- maybe (pure (Fix Nil)) pure mRhs
--         slot <- state (Heap.alloc @Syn rhs)
--         local (Env.insert name slot) rest
--       _ -> rest
--   Prefix Op.Not x ->
--     x >>= pf . Bool . not . truthy . unFix
--   Prefix Op.Neg x ->
--     x >>= \case
--       Fix (Number n) -> pf (Number (negate n))
--       other -> throwError (TypeError "number" (unFix other))

--   If cond act alt -> do
--     should <- truthy . unFix <$> cond
--     case (should, alt) of
--       (True, _) -> act
--       (False, Nothing) -> pure (Fix Nil)
--       (False, Just other) -> other
--   Assign{} -> throwError (Other "unexpected assigment")
--   Var{} -> throwError (Other "unexpected var")
--   Print p -> do
--     p >>= liftIO . print
--     pure (Fix Nil)
--   Return t ->
--   Scope a -> local (Env.push @Addr) a
--   While cond item -> fix $ \recur -> do
--     val <- cond
--     if truthy (unFix val)
--       then item *> recur
--       else pure (Fix Nil)

-- needBool :: Has (Throw EvalError) sig m => (Bool -> Bool -> Bool) -> m Syn -> m Syn -> m Syn
-- needBool fn l r = do
--   a <- l >>= demand "boolean"
--   b <- r >>= demand "boolean"
--   pure (Fix (Bool (fn a b)))

-- needSci :: Has (Throw EvalError) sig m => (Scientific -> Scientific -> a) -> (a -> AST Syn) -> m Syn -> m Syn -> m Syn
-- needSci fn ctor l r = do
--   n <- l >>= demand "number"
--   m <- r >>= demand "number"
--   pure (Fix (ctor (fn n m)))
