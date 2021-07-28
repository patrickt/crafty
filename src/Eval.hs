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

module Eval (eval) where

import AST
import Control.Effect.Error
import Data.Functor.Foldable hiding (Nil)
import Data.HashMap.Strict (HashMap)
import Data.Scientific
import Generic.Data.Orphans ()
import Ident
import Data.Generics.Sum.Constructors
import Optics
import Op qualified

type Bindings = HashMap Ident Syn

data EvalError
  = Unbound Ident
  | TypeError String (AST Syn)
  | Unimplemented Syn
  deriving stock (Eq, Show)

class IsAST a where
  fixed :: Iso' a (AST Syn)

instance IsAST (AST Syn) where fixed = simple
instance IsAST Syn where fixed = coerced

demand ::
  (Has (Throw EvalError) sig m, IsAST ast) =>
  Prism' (AST Syn) a ->
  String ->
  ast ->
  m a
demand p name ast = do
  case ast ^? fixed % p of
    Just a -> pure a
    Nothing -> throwError (TypeError name (ast ^. fixed))

eval :: (Has (Throw EvalError) sig m) => Syn -> m Syn
eval = cataA go
  where
    pf = pure . Fix
    go = \case
      Bool b -> pf (Bool b)
      Nil -> pf Nil
      This -> pf This
      Number n -> pf (Number n)
      Ident i -> pf (Ident i)
      Paren p -> p
      Infix op l r -> case op of
        Op.Or -> needBool (||) l r
        Op.And -> needBool (&&) l r
        Op.Eq -> Fix . Bool <$> ((==) <$> l <*> r)
        Op.Neq -> Fix . Bool <$> ((/=) <$> l <*> r)
        Op.Plus -> needSci (+) Number l r
        Op.Minus -> needSci (-) Number l r
        Op.Mult -> needSci (*) Number l r
        Op.Div -> needSci (/) Number l r
        Op.LT -> needSci (<) Bool l r
        Op.LTE -> needSci (<=) Bool l r
        Op.GT -> needSci (>) Bool l r
        Op.GTE -> needSci (>=) Bool l r
      Prefix Op.Not x ->
        x >>= \case
          Fix (Bool False) -> pf (Bool True)
          Fix (Bool True) -> pf (Bool True)
          _ -> pf (Bool False)
      Prefix Op.Neg x ->
        x >>= \case
          Fix (Number n) -> pf (Number (negate n))
          x -> throwError (TypeError "number" (unFix x))
      x -> sequence x >>= throwError . Unimplemented . Fix

needBool :: Has (Throw EvalError) sig m => (Bool -> Bool -> Bool) -> m Syn -> m Syn -> m Syn
needBool fn l r = do
  a <- l >>= demand (_Ctor @"Bool") "boolean"
  b <- r >>= demand (_Ctor @"Bool") "boolean"
  pure (Fix (Bool (fn a b)))

needSci :: Has (Throw EvalError) sig m => (Scientific -> Scientific -> a) -> (a -> AST Syn) -> m Syn -> m Syn -> m Syn
needSci fn ctor l r = do
  n <- l >>= demand (_Ctor @"Number") "number"
  m <- r >>= demand (_Ctor @"Number") "number"
  pure (Fix (ctor (fn n m)))
