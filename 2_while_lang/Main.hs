module Main where

import Data.Char
import Text.Parsec
import System.Environment
import qualified Data.Map as M
import Eval
import Parser

main = do
     program <- getContents
     case parse whileParser "" program of -- PARSING
        Left e -> print e >> fail "Parse Error"
        Right ast -> solve ans
          where
            ans = M.toList. evalProgram $ ast -- EVALUATING
            solve :: [(String, Integer)] -> IO()
            solve args
              | any ((\v -> v < 0 || v > 10^18). snd) args = error "var is out of range"
              | any (any (not. isLower). fst) args = error "key is out of range"
              | any ((\x -> length x > 10). fst) args = error "key is out of range"
              | otherwise = mapM_ (\(k, v) -> putStrLn $ k ++ " " ++ show v) args
