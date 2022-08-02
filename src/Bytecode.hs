{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Bytecode (module Bytecode) where

import Control.Comonad (extract)
import Control.Effect.Error
import Control.Effect.Optics
import Control.Effect.State
import Control.Effect.Trace
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Optics hiding (assign, modifying, use)
import Stack (Stack)
import Stack qualified
import Prelude hiding (read)

data Op
  = Nop
  | Ret
  | Load
  | Call
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Print
  | Term
  deriving stock (Show, Eq, Ord, Enum, Bounded)

type Value = Int

data VM = VM
  { _ip :: !Int,
    _rip :: !Int,
    _rsp :: !Int,
    _stack :: !(Stack Value),
    _program :: !(Vector Value),
    _constants :: !(Vector Value)
  }
  deriving stock (Show)

makeLenses ''VM

data VMError
  = Terminated
  | BadInstruction Value
  | IPOutOfRange
  | ConstantNotFound
  | StackOverflow
  | StackUnderflow
  deriving stock (Eq, Show)

new :: Int -> Vector Value -> Vector Value -> VM
new n = VM 0 0 0 (Stack.create n 0)

read ::
  ( Has (State VM) sig m,
    MonadIO m,
    Has (Error VMError) sig m
  ) =>
  m Value
read = do
  idx <- use ip
  count <- uses program length
  if
      | idx == count -> throwError Terminated
      | idx > count -> throwError IPOutOfRange
      | idx < 0 -> throwError IPOutOfRange
      | otherwise -> pure ()

  uses program (Vector.! idx)

next :: (Has (State VM) sig m) => m ()
next = ip %= succ

interpret :: (Has (State VM) sig m, Has (Throw VMError) sig m) => Value -> m Op
interpret instrN = do
  let largest = fromEnum (maxBound :: Op)

  when (largest < instrN || instrN < 0) (throwError (BadInstruction instrN))

  pure (toEnum instrN)

-- | Run the provided monadic action and assign it to the target of a 'Setter'.
(<~) :: (Is k A_Setter, Has (State s) sig m) => Optic k is s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)

peek :: Has (State VM) sig m => m Value
peek = uses stack extract

pop :: (Has (Throw VMError) sig m, Has (State VM) sig m) => m Value
pop = do
  item <- peek
  uses stack Stack.safePop >>= maybe (throwError StackUnderflow) (assign stack)
  pure item

push :: (Has (Throw VMError) sig m, Has (State VM) sig m) => Value -> m ()
push a = uses stack (Stack.safePush a) >>= maybe (throwError StackOverflow) (assign stack)

binary :: (Has (Throw VMError) sig m, Has (State VM) sig m) => (Value -> Value -> Value) -> m ()
binary f = go >>= push where go = flip f <$> pop <*> pop

perform ::
  ( Has (State VM) sig m,
    Has (Error VMError) sig m,
    Has Trace sig m,
    MonadIO m
  ) =>
  m Value
perform = step >>= \case
  Term -> peek
  _ -> perform

step ::
  ( Has (State VM) sig m,
    Has (Error VMError) sig m,
    Has Trace sig m,
    MonadIO m
  ) =>
  m Op
step = do
  idx <- read
  trace ("step " <> show idx)
  next

  curr <- interpret idx
  trace ("doing " <> show curr)
  curr <$ case curr of
    Nop -> pure ()
    Neg -> pop >>= push . negate
    Add -> binary (+)
    Sub -> binary (-)
    Mul -> binary (*)
    Div -> binary div
    Term -> pure ()
    Call -> do
      currip <- use rip
      stack %= Stack.push currip

      use rsp >>= push
      rsp <~ use (stack % Stack.sp)

      read >>= assign ip
    Print -> do
      peek >>= liftIO . print
    Ret -> do
      trace "ret"

      back <- use rsp
      stack %= Stack.shrink back
      ip .= back

      rsp <~ pop
      rip <~ pop
    Load -> do
      idx <- read
      trace ("loading constant #" <> show idx)
      next
      uses constants (Vector.!? idx)
        >>= maybe (throwError ConstantNotFound) pure
        >>= push
