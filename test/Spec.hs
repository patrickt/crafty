{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bytecode qualified as VM
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Parser.Church qualified as P
import Control.Carrier.State.Strict
import Control.Carrier.Trace.Returning
import Control.Comonad
import Control.Effect.Parser.Source (Source)
import Control.Monad
import Generators qualified
import Hedgehog
import Parse qualified
import Prettyprinter hiding (annotate)
import Prettyprinter.Render.String
import Optics
import System.Exit (die)
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

prop_vmArithWorks :: Property
prop_vmArithWorks = withTests 1 $ property do
  let constants = [1, 2, 3]
  let prog =
        [ fromEnum VM.Load,
          0,
          fromEnum VM.Load,
          1,
          fromEnum VM.Add,
          fromEnum VM.Term
        ]
  let vm = VM.new 100 prog constants
  (trace, (res, eFinal)) <- runM . runTrace . runState vm . runError @VM.VMError $ VM.perform
  annotateShow res
  footnote (unlines trace)
  res <- evalEither eFinal
  res === 3
