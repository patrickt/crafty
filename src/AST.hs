{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module AST ( AST(..), convert) where

import CST qualified
import Data.Fix (Fix (..))
import Data.Scientific
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import Generic.Data qualified as Data
import Generic.Data.Orphans ()
import GHC.Generics (Generic1)


data AST a
  = Bool Bool
  | Nil
  | This
  | Number Scientific
  | Ident CST.Ident
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
    deriving stock (Functor, Foldable, Traversable, Generic1)
    deriving (Eq1, Show1) via Data.Generically1 AST

instance Show a => Show (AST a) where
  showsPrec = liftShowsPrec showsPrec showList

type Syn = Fix AST

class Convert a where
  convert :: a -> Syn

instance Convert CST.Ident where
  convert = Fix . Ident

instance Convert CST.Primary where
  convert = Fix . \case
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

eval :: MonadFail m => Syn -> m Syn
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
        CST.Or -> needBool (||) l r
        CST.And -> needBool (&&) l r
        CST.Eq -> Fix . Bool <$> ((==) <$> l <*> r)
        CST.Neq -> Fix . Bool <$> ((/=) <$> l <*> r)
        CST.Plus -> needSci (+) Number l r
        CST.Minus -> needSci (-) Number l r
        CST.Mult -> needSci (*) Number l r
        CST.Div -> needSci (/) Number l r
        CST.LT -> needSci (<) Bool l r
        CST.LTE -> needSci (<=) Bool l r
        CST.GT -> needSci (>) Bool l r
        CST.GTE -> needSci (>=) Bool l r
      Prefix CST.Not x -> x >>= \case
        Fix (Bool False) -> pf (Bool True)
        Fix (Bool True) -> pf (Bool True)
        _ -> pf (Bool False)
      Prefix CST.Neg x -> x >>= \case
        Fix (Number n) -> pf (Number (negate n))
        _ -> fail "type error: negate"


      x -> do
        res <- sequence x
        fail ("unimplemented: " <> show res)

needBool :: MonadFail m => (Bool -> Bool -> Bool) -> m Syn -> m Syn -> m Syn
needBool fn l r = do
  Bool a <- unFix <$> l
  Bool b <- unFix <$> r
  pure (Fix (Bool (fn a b)))

needSci :: MonadFail m => (Scientific -> Scientific -> a) -> (a -> AST Syn) -> m Syn -> m Syn -> m Syn
needSci fn ctor l r = do
  Number n <- unFix <$> l
  Number m <- unFix <$> r
  pure (Fix (ctor (fn n m)))
