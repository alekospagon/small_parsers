module Syntax where

-- SYNTAX
data Expr 
	= EAdd Expr Expr
	| ESub Expr Expr
	| EMul Expr Expr
	| EDiv Expr Expr
	| Eint Integer
	deriving (Show)
