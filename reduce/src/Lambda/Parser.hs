module Lambda.Parser (
    term,
) where

import Lambda.Ast (Name, Term(..))

import Text.Parsec (try, many1, skipMany1, chainl1, between, (<|>))
import Text.Parsec.Char (char, space, lower)
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (<*>))

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

identifier :: Parser String
identifier = many1 lower

var :: Parser Term
var = X <$> identifier

lambda :: Parser Term
lambda = Lam <$> (char '\\' >> identifier) <*> (char '.' >> term)

term :: Parser Term
term = (lambda <|> var <|> parens term) `chainl1` (skipMany1 space >> return App)
