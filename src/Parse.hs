{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse (assignment, expression, testParse, reservedWords, parse) where

import CST
import Control.Applicative
import Control.Carrier.Parser.Church qualified as P
import Control.Effect.Parser.Notice
import Control.Effect.Parser.Source
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Prettyprinter (line)
import Prettyprinter.Render.Text
import Text.Parser.Combinators
import Text.Parser.Expression qualified as Expr
import Text.Parser.Token (TokenParsing)
import Text.Parser.Token qualified as Token
import Text.Parser.Token.Style qualified as Token
import Prelude hiding (Ordering (..))

reservedWords :: (IsString s, Hashable s, Eq s) => HashSet s
reservedWords =
  [ "true",
    "false",
    "nil",
    "this",
    "super",
    "!",
    "-",
    "*",
    "/",
    "*",
    "<=",
    ">=",
    "<",
    ">",
    "==",
    "!=",
    "and",
    "or"
  ]

style :: TokenParsing m => Token.IdentifierStyle m
style =
  Token.emptyIdents
    { Token._styleReserved = reservedWords
    }

ident :: (TokenParsing m, Monad m) => m Ident
ident = Id <$> Token.ident style

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = Token.reserve style

primary :: (TokenParsing m, Monad m) => m Expr
primary = prims <|> Token.parens expression <?> "primary expression"
  where
    prims =
      Primary
        <$> choice
          [ Bool True <$ reserved "true",
            Bool False <$ reserved "false",
            Nil <$ reserved "nil",
            This <$ reserved "this",
            Number <$> Token.naturalOrScientific,
            Ident <$> ident,
            Super <$> (reserved "super" *> Token.dot *> ident)
          ]


assignment :: (TokenParsing m, Monad m) => m Expr
assignment = try assign <|> expression
  where
    assign = Assign <$> lhs <*> (reserved "=" *> expression)
    first = Primary . Ident <$> ident
    lhs = foldl' (&) <$> first <*> many (Dot <$$> (Token.dot *> ident))

expression :: (TokenParsing m, Monad m) => m Expr
expression = Expr.buildExpressionParser table call <?> "expression"

(<$$>) :: Functor f => (a -> b -> c) -> f b -> f (a -> c)
a <$$> b = flip a <$> b

table :: (TokenParsing m) => [[Expr.Operator m Expr]]
table =
  [ [ prefix "!" (Prefix Not),
      prefix "-" (Prefix Neg)
    ],
    -- fixme: use Op.associativity here
    [ binary "*" (Infix Mult) Expr.AssocLeft,
      binary "/" (Infix Div) Expr.AssocLeft
    ],
    [ binary "+" (Infix Plus) Expr.AssocLeft,
      binary "-" (Infix Minus) Expr.AssocLeft
    ],
    [ binary "<=" (Infix LTE) Expr.AssocNone,
      binary ">=" (Infix GTE) Expr.AssocNone,
      binary ">" (Infix GT) Expr.AssocNone,
      binary "<" (Infix LT) Expr.AssocNone
    ],
    [ binary "==" (Infix Eq) Expr.AssocLeft,
      binary "!=" (Infix Neq) Expr.AssocLeft
    ],
    [ binary "and" (Infix And) Expr.AssocRight
    ],
    [ binary "or" (Infix Or) Expr.AssocRight
    ]
  ]
  where
    binary name fun = Expr.Infix (fun <$ Token.symbol name)
    prefix name fun = Expr.Prefix (fun <$ Token.symbol name)

--postfix name fun  = Postfix (fun <$ reservedOp name)

call :: (TokenParsing m, Monad m) => m Expr
call = foldl' (&) <$> primary <*> many suffix
  where
    args = Token.parens (Call <$$> (expression `sepBy` Token.comma))
    dot = Dot <$$> (Token.dot *> ident)
    suffix = choice [args, dot]

parse :: P.ParserC (Either (Source, P.Err)) a -> String -> Either (Source, P.Err)  a
parse p s = P.runParserWithString 0 s (Token.whiteSpace *> p <* eof)


testParse :: Show a => P.ParserC (Either (Source, P.Err)) a -> String -> IO ()
testParse p s = do
  let v = parse p s
  either (putDoc . (<> line) . prettyNotice . uncurry P.errToNotice) print v
