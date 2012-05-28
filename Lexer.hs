module Lexer where

import Data.Char as C

data Token = IntType Int 
           | DoubleType Double
           | StringType String
           | BoolType Boolean
           | Symb Char
           | Op Operator
           | For
           | If
           | Else
           | While
           | Do
           | Print
           | Func
           | Error
           | Eof
             
data Operator = Add
              | Sub
              | Mul
              | Div
              | Mod
              | Pow
             
getToken :: String -> (Token, String)
getToken [] = (Eof, [])
getToken str@(x:xs) 
  | C.isDigit x = parseNumber str
  | C.isAlpha x = getString str
  | x `elem` "+-*/^%" = getOperator str
  | otherwise = (Symb x, xs)
                                       
parseNumber :: String -> (Token, String)
parseNumber str = parse str 0 where
  parse [] number = (IntType number, [])
  parse str@(x:xs) number 
    | C.isDigit x = parse xs (number * 10 + (digitToInt x))
    | x == "." = let (fractPart, leftString) = parseRealPart xs 
                    in (DoubleType number + fractPart, leftString)
    | otherwise = (IntType number, str)

parseRealPart :: String -> (Double, String)
parseRealPart str = parse str 0.0 10 where
  parse [] _ _ = 0.0
  parse str@(x:xs) number divider
    | C.isDigit x = parse xs (number + (digitToInt x) / divider) (divider * 10)
    | otherwise = (number, str)
                  
getString :: String -> (Token, String)
getString str = parse str "" where
  parse [] word = (StringType word, [])
  parse str@(x:xs) word
    | !(C.isSpace x) = parse xs (word ++ [x])
    | otherwise = (StringType word, str)
                  
getOperator :: String -> (Token, String)

  