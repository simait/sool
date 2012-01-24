module Syntax where

import PP
import Text.PrettyPrint
import Name
import Type

indent :: [Doc] -> Doc
indent xs = nest 8 $ vcat xs

data Module = Module Name [Object]
	deriving (Show,Eq)

instance PP Module where
	toDoc (Module n os) = text "module" <+> toDoc n <+> lbrace $$ indent (map toDoc os) $$ rbrace

moduleName (Module n _) = n
moduleObjs (Module _ os) = os

data Object = Object Name (Maybe Type) [Decl]
	deriving (Show,Eq)

instance PP Object where
	toDoc (Object n Nothing ds) = text "object" <+> toDoc n <+> lbrace $$ indent (map toDoc ds) $$ rbrace
	toDoc (Object n (Just t) ds) = text "object" <+> toDoc n <+> text "implementing" <+> toDoc t <+> lbrace $$ indent (map toDoc ds) $$ rbrace

objectName (Object n _ _) = n
objectImpl (Object _ i _) = i
objectDecls (Object _ _ ds) = ds

data Decl =
	Decl Type Name
	| MethodDecl (Maybe Type) Name [Decl] [Decl]
	deriving (Show,Eq)

instance PP Decl where
	toDoc (Decl t n) = toDoc t <+> toDoc n
	toDoc (MethodDecl Nothing n ps ds) = text "method" <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$  indent (iterm semi (map toDoc ds)) $$ rbrace
	toDoc (MethodDecl (Just t) n ps ds) = text "method" <+> toDoc t <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (iterm semi (map toDoc ds)) $$ rbrace

declType (Decl t _) = Just t
declType (MethodDecl t _ _ _) = t
declName (Decl _ n) = n
declName (MethodDecl _ n _ _) = n

