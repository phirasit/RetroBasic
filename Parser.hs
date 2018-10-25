module Parser (AST(..), parse) where

import Data.Char (isUpper)

import Scanner

data AST = Null
  | Pgm AST AST
  | Line Token AST
  | Stmt AST
  | Asgmnt Token Token AST
  | Exp1 AST AST
  | Exp21
  | Exp22 Token AST
  | Term Token
  | If AST Token
  | Cond1 AST AST
  | Cond2 Token AST
  | Print Token
  | Goto Token
  | Stop
  deriving (Show)

data NonTerminal = PGM
  | LINE
  | STMT
  | ASGMNT
  | EXP1
  | EXP2
  | TERM
  | IF
  | COND1
  | COND2
  | PRINT
  | GOTO
  | STOP

isLineNum :: Token -> Bool
isLineNum (Scanner.Const n) = 1 <= n && n <= 1000
isLineNum _ = False

isConst :: Token -> Bool
isConst (Scanner.Const n) = 1 <= n && n <= 100
isConst _ = False

isId :: Token -> Bool
isId (Scanner.Id c) = isUpper c
isId _ = False

isTerm :: Token -> Bool
isTerm (Scanner.Id c) = True
isTerm (Scanner.Const n) = True
isTerm _ = False

isArithmeticOp :: Token -> Bool
isArithmeticOp Scanner.Plus = True
isArithmeticOp Scanner.Minus = True
isArithmeticOp _ = False 

isComparator :: Token -> Bool
isComparator Scanner.Less = True
isComparator Scanner.Equal = True
isComparator _ = False


createAST :: NonTerminal -> [Token] -> (AST, [Token])

-- pgm := line pgm | EOF
createAST PGM [] = (Null, [])
createAST PGM tokens@((Scanner.Const _):_) = (Parser.Pgm ast1 ast2, tokens'')
  where 
    (ast1, tokens') = createAST LINE tokens
    (ast2, tokens'') = createAST PGM tokens'
createAST PGM tokens = error $ "Parser: Invalid PGM " ++ (show tokens)

-- line := line_num stmt
createAST LINE (lineNum@(Scanner.Const _):tokens) 
  | isLineNum lineNum = (Parser.Line lineNum ast, tokens')
  | otherwise = error $ "Parser: Invalid LINE " ++ (show lineNum)
  where (ast, tokens') = createAST STMT tokens
createAST LINE tokens = error $ "Parser: Invalid LINE " ++ (show tokens)

-- stmt := asgmnt | if | print | goto | stop
createAST STMT tokens@((Scanner.Id _):_) = (Parser.Stmt ast, tokens')
  where (ast, tokens') = createAST ASGMNT tokens
createAST STMT tokens@(Scanner.If:_) = (Parser.Stmt ast, tokens')
  where (ast, tokens') = createAST IF tokens  
createAST STMT tokens@(Scanner.Print:_) = (Parser.Stmt ast, tokens')
  where (ast, tokens') = createAST PRINT tokens  
createAST STMT tokens@(Scanner.Goto:_) = (Parser.Stmt ast, tokens')
  where (ast, tokens') = createAST GOTO tokens  
createAST STMT tokens@(Scanner.Stop:_) = (Parser.Stmt ast, tokens')
  where (ast, tokens') = createAST STOP tokens
createAST STMT tokens = error $ "Parser: Invalid STMT " ++ (show tokens)

-- asgmnt := id = exp1
createAST ASGMNT (c:Scanner.Equal:tokens) 
  | isId c = (Parser.Asgmnt c Scanner.Equal ast, tokens')
  | otherwise = error $ "Parser: Invalid ASGMNT " ++ (show c)
  where (ast, tokens') = createAST EXP1 tokens
createAST ASGMNT tokens = error $ "Parser: Invalid ASGMNT " ++ (show tokens)

-- exp1 := term exp2 
createAST EXP1 tokens@(c:_) 
  | isTerm c = (Parser.Exp1 ast1 ast2, tokens'') 
  | otherwise = error $ "Parser: Invalid EXP1 " ++ (show c)
  where 
    (ast1, tokens') = createAST TERM tokens
    (ast2, tokens'') = createAST EXP2 tokens'
createAST EXP1 tokens = error $ "Parser: Invalid EXP1 " ++ (show tokens)

-- exp2 := empty | + term | - term
createAST EXP2 [] = (Parser.Exp21, [])
createAST EXP2 tokens@((Scanner.Const _):_) = (Parser.Exp21, tokens)
createAST EXP2 (Scanner.Plus:tokens) = (Parser.Exp22 Scanner.Plus ast, tokens')
  where (ast, tokens') = createAST TERM tokens
createAST EXP2 (Scanner.Minus:tokens) = (Parser.Exp22 Scanner.Minus ast, tokens')
  where (ast, tokens') = createAST TERM tokens
createAST EXP2 tokens = error $ "Parser: Invalid EXP2 " ++ (show tokens)

-- term := id | const
createAST TERM (c:tokens) 
  | isTerm c = (Parser.Term c, tokens)
  | otherwise = error $ "Parser: Invalid TERM " ++ (show c)
createAST TERM tokens = error $ "Parser: Invalid TERM " ++ (show tokens)

-- if := IF cond1 line_num
createAST IF (Scanner.If:tokens)
  | isLineNum lineNum = (Parser.If ast lineNum, tokens')
  | otherwise = error $ "Parser: Invalid If " ++ (show lineNum)
  where (ast, (lineNum:tokens')) = createAST COND1 tokens
createAST IF tokens = error $ "Parser: Invalid IF " ++ (show tokens)

-- cond1 :- term cond2
createAST COND1 tokens@((Scanner.Const _):_) = (Parser.Cond1 ast1 ast2, tokens'')
  where
    (ast1, tokens') = createAST TERM tokens
    (ast2, tokens'') = createAST COND2 tokens'
createAST COND1 tokens = error $ "Parser: Invalid COND1 " ++ (show tokens)

-- cond2 :- < term | = term
createAST COND2 (Scanner.Less:tokens) = (Parser.Cond2 Scanner.Less ast, tokens')
  where (ast, tokens') = createAST TERM tokens
createAST COND2 (Scanner.Equal:tokens) = (Parser.Cond2 Scanner.Equal ast, tokens')
  where (ast, tokens') = createAST TERM tokens
createAST COND2 tokens = error $ "Parser: Invalid COND2 " ++ (show tokens)

-- print := PRINT id
createAST PRINT (Scanner.Print:c:tokens) 
  | isId c = (Parser.Print c, tokens)
  | otherwise = error $ "Parser: Invalid PRINT " ++ (show c)
createAST PRINT tokens = error $ "Parser: Invalid PRINT " ++ (show tokens)

-- goto := GOTO line_num
createAST GOTO (Scanner.Goto:lineNum:tokens)
  | isLineNum lineNum = (Parser.Goto lineNum, tokens)
  | otherwise = error $ "Parser: Invalid GOTO " ++ (show lineNum)
createAST GOTO tokens = error $ "Parser: Invalid GOTO "++ (show tokens)

-- stop := STOP
createAST STOP (Scanner.Stop:tokens) = (Parser.Stop, tokens)
createAST STOP tokens = error $ "Parser: Invalid STOP " ++ (show tokens)


--- Parser API ---
parse :: [Token] -> AST
parse tokens 
  | null tokens' = ast
  | otherwise = error $ "Parser: Invalid parse " ++ (show tokens) ++ "\nLeft with:" ++ (show tokens')
  where (ast, tokens') = createAST PGM tokens

