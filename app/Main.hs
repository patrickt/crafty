{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad
import System.Environment (getArgs)
import Parse qualified
import System.Exit (die)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die "No arguments provided.")
  case Parse.parse Parse.expression (head args) of
    Left e -> print e *> die "Parsing failed!"
    Right e -> putDoc (pretty e)
