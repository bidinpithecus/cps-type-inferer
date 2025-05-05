module Lambda.Parser where

import Lambda.Typing
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

parseExpression :: String -> Either ParseError Expr
parseExpression = parse (spacesAndComments *> parseExpr <* eof) ""

parseExpr :: Parser Expr
parseExpr = parseLet <|> parseLam <|> parseAppChain

parseTerm :: Parser Expr
parseTerm = parens parseExpr <|> parseVar

parseVar :: Parser Expr
parseVar = identifier >>= \i -> return (Var i) <?> "variable"

parseLam :: Parser Expr
parseLam = do
    vars <- try $ do
        _ <- symbol "\\" <|> symbol "λ"
        vars <- many1 identifier
        _ <- symbol "."
        return vars
    body <- parseExpr
    return (foldr Lam body vars) <?> "lambda abstraction"

parseAppChain :: Parser Expr
parseAppChain = do
    terms <- many1 parseTerm
    return (foldl1 App terms) <?> "application"

parseLet :: Parser Expr
parseLet = try $ do
    reserved "let"
    name <- identifier
    reservedOp "="
    value <- parseExpr
    reserved "in"
    Let name value <$> parseExpr
