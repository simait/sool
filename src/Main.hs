module Main where

import PP
import Text.PrettyPrint
import Lexer
import Parser

main = do
	input <- getContents
	let parseTree = mdl (alexScanTokens input)
	--putStrLn ("parseTree: " ++ (show parseTree))
	putStrLn (render (toDoc (toSyntax parseTree)))
	--print "done"
