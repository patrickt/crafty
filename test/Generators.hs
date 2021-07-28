{-# LANGUAGE ImportQualifiedPost #-}

module Generators (module Generators) where

import CST
import Control.Monad
import Hedgehog
import Op qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parse qualified
import Data.Scientific (Scientific, scientific)

ident :: Gen Ident
ident = do
  name <- Gen.text (Range.linear 3 8) Gen.lower
  guard (name `notElem` Parse.reservedWords)
  pure (Id name)

infix' :: Gen Infix
infix' = do
  b <- Gen.enumBounded
  pure b-- <$ guard (Op.associativity b /= Op.AssocNone)

prefix :: Gen Prefix
prefix = Gen.enumBounded

primary :: Gen Primary
primary =
  Gen.choice
    [ Bool <$> Gen.bool,
      pure Nil,
      pure This,
      Number . Left <$> Gen.integral (Range.linear 0 99999999999),
      Number . Right <$> decimal (Range.linear 0 1000) (Range.linear (-5) 20),
      Ident <$> ident,
      Super <$> ident
    ]

lhs :: Gen Expr
lhs =
  Gen.recursive
    Gen.choice
    [ Primary . Ident <$> ident
    ]
    [ Gen.subtermM lhs (\a -> (Dot a) <$> ident)
    ]

decimal :: Range Integer -> Range Int -> Gen Scientific
decimal a b = scientific <$> Gen.integral a <*> Gen.integral b

expr :: Gen Expr
expr =
  Gen.recursive
    Gen.choice
    [ Primary <$> primary
    ]
    [ Gen.subtermM2 expr expr (\a b -> Infix <$> infix' <*> pure a <*> pure b)
    , Gen.subtermM expr (\a -> Dot a <$> ident)
    , Gen.subtermM expr (\a -> Prefix <$> prefix <*> pure a)
    , Gen.subterm expr (`Call` [])
    , Gen.subterm2 expr expr (\a b -> Call a [b])
    , Gen.subterm3 expr expr expr (\a b c -> Call a [b, c])
    ]
