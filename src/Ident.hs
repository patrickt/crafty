{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ident (Ident (..)) where

import Data.Text (Text)
import Data.String (IsString)
import Prettyprinter (Pretty)
import Data.Hashable (Hashable)

newtype Ident = Id Text
  deriving stock (Eq, Show)
  deriving newtype (Hashable, IsString, Pretty)
