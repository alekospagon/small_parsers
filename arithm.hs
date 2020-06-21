import Text.ParserCombinators.Parsec
import Control.Monad.Trans
import System.Console.Haskeline
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Number (int)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Data.Functor.Identity


-- SYNTAX
data Expr 
	= EAdd Expr Expr
	| ESub Expr Expr
	| EMul Expr Expr
	| EDiv Expr Expr
	| Eint Integer
	deriving (Show)



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
	,	Tok.reservedOpNames = ["*", "/", "+", "-"]
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



-- EVAL
eval x = case x of 
	EMul a1 a2 -> eval a1 * eval a2
	EDiv a1 a2 -> eval a1 `div` eval a2
	EAdd a1 a2 -> eval a1 + eval a2
	ESub a1 a2 -> eval a1 - eval a2
	Eint a     -> a 




-- IO HELPER. PARSE & EVALUATE
process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> print $ eval ex



main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Expr> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
