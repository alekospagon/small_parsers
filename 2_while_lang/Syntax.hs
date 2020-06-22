module Syntax ( Opa (..) , Opb (..), Opr(..), AExpr (..) , 
	BExpr (..), Stmt (..) ) where 

data Opa 
	= Add
	| Sub
	| Mul 
	| Div
	deriving Show

data Opb 
	= And
	| Or 
	deriving Show

data Opr 
	= Greater
	| Less
	deriving Show

data AExpr 
	= Var String
	| Num Integer
	| Abin Opa AExpr AExpr
	deriving Show

data BExpr 
	= Con Bool
	| BBin Opb BExpr BExpr
	| AL Opr AExpr AExpr
	deriving Show

data Stmt 
	= List [ Stmt ]
	| Assing AExpr AExpr
	| If BExpr Stmt Stmt
	| While BExpr Stmt 
	deriving Show
