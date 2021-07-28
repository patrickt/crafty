{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ident (Ident (..)) where

import Data.Text (Text)
import Data.String (IsString)
import Prettyprinter (Pretty)

newtype Ident = Id Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Pretty)
