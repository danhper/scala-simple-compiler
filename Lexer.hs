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
getToken [] = Eof
getToken str@(head:tail) 
  | C.isDigit head = parseNumber str
  | C.isAlpha head = getString str
  | head `elem` "+-*/^%" = getOperator str
  | otherwise = (Symb head, tail)
                                       
parseNumber :: String -> (Token, String)
parseNumber str = parse str 0 where
  parse [] _ = error "Missing operand"
  parse str@(head:tail) number 
    | C.isDigit head = parse tail (number * 10 + (digitToInt head))
    | head == "." = let (fractPart, leftString) = parseRealPart tail 
                    in (DoubleType number + fractPart, leftString)  
    | otherwise = (IntType number, str)

parseRealPart :: String -> (Double, String)
parseRealPart str = parse str 0.0 10 where
  parse [] _ _ = 0.0
  parse (head:tail) number divider
    | C.isDigit head = parse tail (number / divider) (divider * 10)