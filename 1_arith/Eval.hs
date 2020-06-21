module Eval where

import Syntax

import Data.Maybe
import Data.Functor

-- EVAL
eval x = case x of 
	EMul a1 a2 -> eval a1 * eval a2
	EDiv a1 a2 -> eval a1 `div` eval a2
	EAdd a1 a2 -> eval a1 + eval a2
	ESub a1 a2 -> eval a1 - eval a2
	Eint a     -> a 
