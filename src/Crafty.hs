{- |
Copyright: (c) 2021 Patrick Thomson
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Patrick Thomson <patrickt@github.com>

An implementation of the Lox language from Crafting Interpreters.
-}

module Crafty
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
