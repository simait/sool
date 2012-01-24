module Name where

import PP
import Text.PrettyPrint

data Name = Name String
	deriving (Show,Eq)

instance PP Name where
	toDoc (Name n) = text n

nameStr (Name s) = s
mkName s = (Name s)

data QName = QName String String
	deriving (Show,Eq)

instance PP QName where
	toDoc (QName p n) = text (p++n)

qNamePrefix (QName p _) = p
qNameStr (QName _ s) = s
mkQName p n = QName p n