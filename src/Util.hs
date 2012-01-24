module Util where

import PP
import Text.PrettyPrint

data Lit =
	LInt Int				|
	LChar Char				|
	LString String
	deriving (Show, Eq)


instance PP Lit where
	toDoc (LInt i) = int i
	toDoc (LChar c) = char c
	toDoc (LString s) = doubleQuotes (text s)