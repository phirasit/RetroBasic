import Data.List
import System.IO

import Scanner
import Parser
import Compiler

main :: IO() 

main = do
  
  -- get input information
  hPutStr stderr "Input file name: "
  hFlush stderr 
  fileName <- getLine
  
  -- read input file
  hPutStrLn stderr $ "#### Reading from " ++ (show fileName) ++ " ####"
  input <- readFile fileName

  -- display the input
  hPutStrLn stderr "#### Input ####"
  hPutStr stderr input

  -- tokenize the input
  let tokens = scan input
  hPutStrLn stderr "#### Tokenized Input ####"
  hPutStrLn stderr $ show tokens

  -- parse the input
  let tree = parse tokens
  hPutStrLn stderr "#### Parsed Input ####"
  hPutStrLn stderr $ show tree

  let bCode = toBCode tree
  hPutStrLn stderr "#### Compiled B-code ####"
  mapM_ (putStrLn . intercalate " " . map show) bCode
