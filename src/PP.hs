module PP where

import Data.List
import Text.PrettyPrint

isep :: Doc -> [Doc] -> [Doc]
isep s ds = intersperse s ds

iterm :: Doc -> [Doc] -> [Doc]
iterm d ds = map (\x-> x <> d) ds

class PP a where
	toDoc :: a -> Doc