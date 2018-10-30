module Scanner (Token(..), scan) where

import Data.Char (isDigit, isSpace, isUpper)

data Token = Const Int
  | Id Char
  | If
  | Goto
  | Print
  | Stop
  | Plus
  | Minus
  | Less
  | Equal
  deriving (Show)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

tokenize :: String -> Token
tokenize "IF" = If
tokenize "GOTO" = Goto
tokenize "PRINT" = Print
tokenize "STOP" = Stop
tokenize "+" = Plus
tokenize "-" = Minus
tokenize "<" = Less
tokenize "=" = Equal
tokenize w@(x:xs) 
  | isDigit x = Const $ (read :: String -> Int) w 
  | isUpper x && null xs = Id x
  | otherwise = error $ "Scanner: Token Unrecognize " ++ (show x) ++ " " ++ (show xs)

tokenize t = error $ "Scanner: Token Unrecognize " ++ (show t)

scan :: String -> [Token]
scan w = map tokenize $ filter (/="") $ map trim $ words w

