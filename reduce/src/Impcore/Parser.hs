module Impcore.Parser (
    imp,
) where

import Impcore.Ast (
  Name,
  Value,
  XDef(..),
  UnitTest(..),
  Def(..),
  Exp(..)
  )

import Text.Parsec (try, many, notFollowedBy, (<|>))
import Text.Parsec.Char (char, satisfy)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as P
import Control.Monad (mzero)
import Data.Char (isSpace)

import Prelude hiding (exp)

identOk :: Char -> Bool
identOk c = not $ c `elem` "();" || isSpace c

lispStyle :: LanguageDef st
lispStyle = emptyDef {
                commentLine    = ";",
                nestedComments = True,
                identStart     = identLetter lispStyle,
                identLetter    = satisfy identOk,
                opStart        = opLetter lispStyle,
                opLetter       = mzero,
                reservedOpNames= [],
                reservedNames  = [],
                caseSensitive  = True
            }

lexer = P.makeTokenParser lispStyle

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
reserved = P.reserved lexer
identifier = notFollowedBy numeral >> P.identifier lexer
sign = (char '-' >> return negate)
        <|> (char '+' >> return id)
        <|> return id
lexeme = P.lexeme lexer
numeral = sign >>= (<$> lexeme (P.decimal lexer))

filename :: Parser Name
filename = identifier

varname :: Parser Name
varname = identifier

funname :: Parser Name
funname = identifier

formals :: Parser [Name]
formals = many varname

set :: Parser Exp
set = reserved "set" >> Set <$> varname <*> exp

ifx :: Parser Exp
ifx = reserved "if" >> If <$> exp <*> exp <*> exp

while :: Parser Exp
while = reserved "while" >> While <$> exp <*> exp

begin :: Parser Exp
begin = reserved "begin" >> Begin <$> many exp

apply :: Parser Exp
apply = Apply <$> funname <*> many exp

literal :: Parser Exp
literal = Literal <$> numeral

variable :: Parser Exp
variable = Var <$> varname

exp :: Parser Exp
exp = parens (set <|> ifx <|> while <|> begin <|> apply) <|> literal <|> variable

val :: Parser Def
val = reserved "val" >> Val <$> varname <*> exp

define :: Parser Def
define = reserved "define" >> Define <$> funname <*> parens formals <*> exp

expdef :: Parser Def
expdef = Exp <$> exp

def :: Parser Def
def = try (parens (val <|> define)) <|> expdef

checkexpect :: Parser UnitTest
checkexpect = reserved "check-expect" >> CheckExpect <$> exp <*> exp

checkerror :: Parser UnitTest
checkerror = reserved "check-error" >> CheckError <$> exp

unittest :: Parser UnitTest
unittest = parens (checkexpect <|> checkerror)

defx :: Parser XDef
defx = Def <$> def

unittestx :: Parser XDef
unittestx = Test <$> unittest

use :: Parser XDef
use = reserved "use" >> Use <$> filename

xdef :: Parser XDef
xdef = try (parens use) <|> try unittestx <|> defx

imp :: Parser [XDef]
imp = whiteSpace >> many xdef
