module Compiler (toBCode) where

import Control.Exception.Base
import Data.Char (ord)

import Scanner
import Parser

lineCode    = 10
idCode      = 11
constCode   = 12
ifCode      = 13
gotoCode    = 14
printCode   = 15
stopCode    = 16
opCode      = 17

opPlusCode  =  1
opMinusCode =  2
opLessCode  =  3
opEqualCode =  4


ref :: Char -> Int
ref c = (ord c) - (ord 'A') + 1 


showLineCode :: Int -> [Int]
showLineCode n = lineCode : [n]

showIdCode :: Char -> [Int]
showIdCode c = idCode : [ref c]

showConstCode :: Int -> [Int]
showConstCode n = constCode : [n]

showIfCode :: [Int]
showIfCode = ifCode : [0]

showGotoCode :: Int -> [Int]
showGotoCode n = gotoCode : [n]

showPrintCode :: [Int]
showPrintCode = printCode : [0]

showStopCode :: [Int]
showStopCode = stopCode : [0]

showOpCode :: Token -> [Int]
showOpCode Scanner.Plus = opCode : [opPlusCode]
showOpCode Scanner.Minus = opCode : [opMinusCode]
showOpCode Scanner.Less = opCode : [opLessCode]
showOpCode Scanner.Equal = opCode : [opEqualCode]
showOpCode _ = assert False []



toBCodeInline :: AST -> [Int]

toBCodeInline (Parser.Line (Scanner.Const n) ast1) = lineCode : n : (toBCodeInline ast1)
toBCodeInline (Parser.Line _ _) = assert False []

toBCodeInline (Parser.Stmt ast1) = toBCodeInline ast1

toBCodeInline (Parser.Asgmnt (Scanner.Id c) Scanner.Equal ast1) = (showIdCode c) ++ (showOpCode Scanner.Equal) ++ (toBCodeInline ast1)
toBCodeInline (Parser.Asgmnt _ _ _) = assert False []

toBCodeInline (Parser.Exp1 ast1) = toBCodeInline ast1
toBCodeInline (Parser.Exp2 ast1 op ast2) = (toBCodeInline ast1) ++ (showOpCode op) ++ (toBCodeInline ast2)

toBCodeInline (Parser.Term (Scanner.Id c)) = showIdCode c
toBCodeInline (Parser.Term (Scanner.Const n)) = showConstCode n
toBCodeInline (Parser.Term _) = assert False []

toBCodeInline (Parser.If ast1 (Scanner.Const n)) = showIfCode ++ (toBCodeInline ast1) ++ (showGotoCode n)
toBCodeInline (Parser.If _ _) = assert False []

toBCodeInline (Parser.Cond ast1 op ast2) = (toBCodeInline ast1) ++ (showOpCode op) ++ (toBCodeInline ast2) 

toBCodeInline (Parser.Print (Scanner.Id c)) = showPrintCode ++ (showIdCode c)
toBCodeInline (Parser.Print _) = assert False []

toBCodeInline (Parser.Goto (Scanner.Const n)) = showGotoCode n
toBCodeInline (Parser.Goto _) = assert False []

toBCodeInline Parser.Stop = showStopCode

toBCodeInline _ = assert False []



-- Compiler API 
toBCode :: AST -> [[Int]]

toBCode (Parser.Pgm ast1 ast2) = (toBCodeInline ast1) : (toBCode ast2)

toBCode Parser.Null = [[0]]

toBCode _ = assert False []


