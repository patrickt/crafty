{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Parse where

import Control.Carrier.Parser.Church qualified as P
import Control.Effect.Parser.Notice
import Control.Effect.Parser.Source
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Monoid
import Data.Scientific (Scientific)
import Data.String (IsString)
import Data.Text (Text)
import Prettyprinter (line)
import Prettyprinter.Render.Text
import Text.Parser.Combinators
import Text.Parser.Expression qualified as Expr
import Text.Parser.Token (TokenParsing)
import Text.Parser.Token qualified as Token
import Text.Parser.Token.Style qualified as Token

style :: TokenParsing m => Token.IdentifierStyle m
style =
  Token.emptyIdents
    { Token._styleReserved = ["true", "false", "nil", "this", "super"]
    }

newtype Ident = Id Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

ident :: (TokenParsing m, Monad m) => m Ident
ident = Id <$> Token.ident style

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = Token.reserve style

data Primary
  = Bool Bool
  | Nil
  | This
  | Number (Either Integer Scientific)
  | Ident Ident
  | Paren Expr
  | Super Ident
  deriving stock (Show)

primary :: (TokenParsing m, Monad m) => m Expr
primary =
  Primary
    <$> choice
      [ Bool True <$ reserved "true",
        Bool False <$ reserved "false",
        Nil <$ reserved "nil",
        This <$ reserved "this",
        Number <$> Token.naturalOrScientific,
        Ident <$> ident,
        Paren <$> Token.parens expression,
        Super <$> (reserved "super" *> Token.dot *> ident)
      ]
    <?> "primary expression"

data Infix = Or | And | Eq | Neq | Plus | Minus | Mult | Div | LT | LTE | GT | GTE
  deriving stock (Show)

data Prefix = Not | Neg
  deriving stock (Show)

data Expr
  = Assign (Maybe Expr) Ident Expr
  | Infix Infix Expr Expr
  | Dot Expr Ident
  | Prefix Prefix Expr
  | Call Expr [Expr]
  | Primary Primary
  deriving stock (Show)

expression :: (TokenParsing m, Monad m) => m Expr
expression = Expr.buildExpressionParser table call <?> "expression"

(<$$>) :: Functor f => (a -> b -> c) -> f b -> f (a -> c)
a <$$> b = flip a <$> b

table :: (TokenParsing m, Monad m) => [[Expr.Operator m Expr]]
table =
  [ [ prefix "!" (Prefix Not),
      prefix "-" (Prefix Neg)
    ],
    [ binary "*" (Infix Mult) Expr.AssocLeft,
      binary "/" (Infix Div) Expr.AssocLeft
    ]
  ]
  where
    binary name fun = Expr.Infix (fun <$ reserved name)
    prefix name fun = Expr.Prefix (fun <$ reserved name)

--postfix name fun  = Postfix (fun <$ reservedOp name)

call :: (TokenParsing m, Monad m) => m Expr
call = do
  lhs <- primary
  rhs <-
    many
      ( choice
          [ Token.parens (Call <$$> (expression `sepBy` Token.comma)),
            Dot <$$> (Token.dot *> ident)
          ]
      )
  pure (foldl' (&) lhs rhs)

parse :: Show a => P.ParserC (Either (Source, P.Err)) a -> String -> IO ()
parse p s = do
  let v = P.runParserWithString 0 s p
  either (putDoc . (<> line) . prettyNotice . uncurry P.errToNotice) print v
