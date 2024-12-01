module Lexer where

import qualified Data.Functor.Identity
import Text.Parsec (ParsecT)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as L

lexical :: L.GenTokenParser String u Data.Functor.Identity.Identity
lexical = L.makeTokenParser emptyDef

symbol :: String -> ParsecT String u Data.Functor.Identity.Identity String
symbol = L.symbol lexical

parens :: ParsecT String u Data.Functor.Identity.Identity a -> ParsecT String u Data.Functor.Identity.Identity a
parens = L.parens lexical

identifier :: ParsecT String u Data.Functor.Identity.Identity String
identifier = L.identifier lexical

spacesAndComments :: ParsecT String u Data.Functor.Identity.Identity ()
spacesAndComments = L.whiteSpace lexical
