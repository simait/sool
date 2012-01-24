module Main where

import Syntax
import Parser

main = do
	input <- getContents
	let syntax = parseMDL input
	let pretty = prettySyntax syntax
	putStrLn pretty