{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad
import Generators qualified
import Hedgehog
import Prettyprinter
import Prettyprinter.Render.String
import System.Exit (die)
import Control.Carrier.Parser.Church qualified as P
import Parse qualified
import Control.Effect.Parser.Source (Source)
import Text.Parser.Combinators (eof)

main :: IO ()
main = do
  t <- tests
  unless t (die "tests failed")

tests :: IO Bool
tests = checkParallel $$(discover)

prop_roundtripParsePretty :: Property
prop_roundtripParsePretty = property do
  expr <- forAll Generators.expr
  let rendering = renderString . layoutPretty defaultLayoutOptions . pretty
  let parsing s = P.runParserWithString 0 s (Parse.assignment @(P.ParserC (Either (Source, P.Err))) <* eof)
  tripping expr rendering parsing
