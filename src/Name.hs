module Name where

import PP
import Text.PrettyPrint

data Name = Name String
	deriving (Show,Eq)

data QName = QName String String
	deriving (Show,Eq)

instance PP QName where
	toDoc (QName p n) = text (p++n)

instance PP Name where
	toDoc (Name n) = text n

mkName s = (Name s)