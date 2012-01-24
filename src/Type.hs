module Type where

import PP
import Text.PrettyPrint

data Type = Type String
	deriving (Show,Eq)

mkType n = Type n

instance PP Type where
	toDoc (Type n) = text n
