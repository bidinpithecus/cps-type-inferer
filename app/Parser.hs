module Parser where

import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)
import Typing

parseExpression :: String -> Either ParseError Expr
parseExpression = parse parseExpr "Erro:"

parseExpr :: Parser Expr
parseExpr = try parseApp <|> parseLam <|> parseVar <|> parens parseExpr

parseVar :: Parser Expr
parseVar = identifier >>= \i -> return $ Var i

parseLam :: Parser Expr
parseLam =
  do
    _ <- symbol "\\"
    i <- identifier
    _ <- symbol "."
    Lam i <$> parseExpr

parseSimpleExpr :: Parser Expr
parseSimpleExpr = parseVar <|> parens parseExpr

parseApp :: Parser Expr
parseApp =
  do
    e1 <- parseSimpleExpr
    e2 <- many1 parseSimpleExpr
    return $ foldl App e1 e2
