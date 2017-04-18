module Main where

import LexMinisculus
import ParMinisculus
import AST
import ASTConverter
import AbsMinisculus
import System.Environment
import ErrM
import qualified Text.Show.Pretty as Pretty

main = do
	args <- getArgs
	let fname = args !! 0
	fConts <- readFile fname
	let
	 tokens = myLexer fConts
	 pTree  = pProg tokens
	case pTree of 
		Ok rpTree -> do 
			let 
			   ast = transProg rpTree
			putStrLn $ Pretty.ppShow ast
		Bad s -> error $ "Eror in parsing: " ++ s 
