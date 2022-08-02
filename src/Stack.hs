{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
module Stack
  ( Stack (Stack)
  , create
  , push
  , safePush
  , safePop
  , pop
  , sp
  , extract
  , shrink
  , contents
  )
  where

import Control.Comonad
import Data.Vector (Vector, (//))
import Data.Vector qualified as Vector
import Optics

data Stack a = Stack
  { _sp :: Int
  , _contents :: Vector a
  } deriving stock (Show, Functor)

instance Comonad Stack where
  extract (Stack s a) = a Vector.! s
  duplicate s@(Stack sp _) = Stack sp (Vector.replicate sp s)

makeLenses ''Stack

create :: Int -> a -> Stack a
create n = Stack 0 . Vector.replicate n

push :: a -> Stack a -> Stack a
push a (Stack p c) = Stack (p + 1) (c // [(p + 1, a)])

safePush :: a -> Stack a -> Maybe (Stack a)
safePush a s@(Stack p c)
  | p == length c = Nothing
  | otherwise = Just (push a s)

pop :: Stack a -> Stack a
pop (Stack p c) = Stack (p - 1) c

safePop :: Stack a -> Maybe (Stack a)
safePop (Stack 0 _) = Nothing
safePop s = Just (pop s)

shrink :: Int -> Stack a -> Stack a
shrink = set sp
