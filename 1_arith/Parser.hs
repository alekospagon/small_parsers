module Parser (
  parseExpr
) where

import Syntax

import Text.Parsec
import Text.Parsec.Number (int)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

-- LEXER 
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef 
	{	Tok.commentStart = ""
	,	Tok.commentEnd   = ""
	, 	Tok.commentLine  = ""
	,	Tok.nestedComments = True
	,	Tok.identStart = letter
	,	Tok.identLetter = letter
	,	Tok.opStart = oneOf ""
	, 	Tok.opLetter = oneOf ""
	,	Tok.reservedNames = ["while", "do"]
	,	Tok.reservedOpNames = ["*", "/", "+", "-", ":="]
	,	Tok.caseSensitive = False
	}

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer



-- OPERATORS
infixOp name fun assoc = Ex.Infix (do { reservedOp name; return fun }) assoc
table = 
	[
	[ infixOp "*" EMul Ex.AssocLeft,	infixOp "/" EDiv Ex.AssocLeft] ,
	[ infixOp "+" EAdd Ex.AssocLeft,	infixOp "-" ESub Ex.AssocLeft]	
	]

-- TERMS
take_num = do 
	whiteSpace 
	num <- int
	whiteSpace
	return $ Eint num

term = take_num <|> parens expr


-- EXPRESION BUILDER
expr :: Parser Expr
expr = Ex.buildExpressionParser table term


-- PARSE EXPRESSION
contents :: Parser a -> Parser a
contents p = do
	r <- p 
	eof
	return r

parseExpr s = parse (contents expr) "<stdin>" s
