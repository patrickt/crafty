{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Heap
  ( Heap,
    Addr,
    unallocated,
    count,
    initial,
    alloc,
    update,
    lookup,
    addresses,
  )
where

import Data.Monoid
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Prettyprinter (Pretty (..))
import Generic.Data.Orphans ()
import Prettyprinter qualified as Pretty
import Data.Function
import Prelude hiding (lookup)
import GHC.Generics (Generic)
import Data.Data qualified as Data

newtype Addr = Addr Int deriving stock (Eq, Ord, Data.Data)

unallocated :: Addr
unallocated = Addr (negate 1)

instance Show Addr where show (Addr a) = '#' : show a

instance Pretty Addr where pretty = Pretty.viaShow

data Heap a = Heap
  { count :: Int,
    entries :: IntMap a
  }
  deriving stock (Generic)


instance Eq a => Eq (Heap a) where (==) = (==) `on` entries

instance Show a => Show (Heap a) where show = show . entries

instance Pretty a => Pretty (Heap a) where
  pretty (Heap _ e) = do
    let pair (a, b) = pretty a <> ": " <> pretty b
    let elements = fmap pair (IM.toList e)
    Pretty.align (Pretty.list elements)

initial :: Heap a
initial = Heap 0 mempty

alloc :: a -> Heap a -> (Heap a, Addr)
alloc x (Heap c es) = (Heap (succ c) new, Addr c)
  where
    new = IM.insert c x es

update :: Addr -> a -> Heap a -> Heap a
update (Addr a) n h = h {entries = IM.insert a n (entries h)}

lookup :: Addr -> Heap a -> Maybe a
lookup (Addr a) h = IM.lookup a (entries h)

addresses :: Heap a -> [Addr]
addresses = fmap Addr . IM.keys . entries
