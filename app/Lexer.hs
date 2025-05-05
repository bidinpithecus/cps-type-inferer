module Lexer where

import Text.Parsec (Parsec, letter, char, (<|>), alphaNum, oneOf, (<?>))
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexical :: Token.TokenParser ()
lexical = Token.makeTokenParser style
  where
    style = emptyDef {
        Token.identStart = letter <|> char '_',
        Token.identLetter = alphaNum <|> oneOf "_'",
        Token.reservedOpNames = ["\\", "λ", ".", "=", "->", "let", "in"],
        Token.reservedNames = ["let", "in"],
        Token.commentLine = "--"
    }

symbol :: String -> Parsec String () String
symbol = Token.symbol lexical

parens :: Parsec String () a -> Parsec String () a
parens = Token.parens lexical

identifier :: Parsec String () String
identifier = Token.identifier lexical <?> "identifier"

spacesAndComments :: Parsec String () ()
spacesAndComments = Token.whiteSpace lexical

reserved :: String -> Parsec String () ()
reserved = Token.reserved lexical

reservedOp :: String -> Parsec String () ()
reservedOp = Token.reservedOp lexical

commaSep :: Parsec String () a -> Parsec String () [a]
commaSep = Token.commaSep lexical

semiSep :: Parsec String () a -> Parsec String () [a]
semiSep = Token.semiSep lexical
