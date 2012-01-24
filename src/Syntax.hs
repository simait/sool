module Syntax where

import PP
import Text.PrettyPrint
import Name
import Type
import Util

indent :: [Doc] -> Doc
indent xs = nest 8 $ vcat xs

data Module = Module Name [Decl]
	deriving (Show,Eq)

data Decl =
	DClass Name [Decl] (Maybe Type) [Decl]	|
	DVariable Type Name			|
	DMethod (Maybe Type) Name [Decl] [Cmd]
	deriving (Show,Eq)

data Cmd =
	CDecl Type Name				|
	CAssign Name Expr			|
	CInvoke (Maybe Name) Name [Expr]
	deriving (Show, Eq)

data Expr =
	ELit Lit				|
	EVariable Name
	deriving (Show, Eq)


instance PP Module where
	toDoc (Module n os) = text "module" <+> toDoc n <+> lbrace $$ indent (map toDoc os) $$ rbrace

instance PP Decl where
	toDoc (DClass n ps Nothing ds) = text "class" <+> toDoc n <> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (map toDoc ds) $$ rbrace
	toDoc (DClass n ps (Just t) ds) = text "class" <+> toDoc n <> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> text "implementing" <+> toDoc t <+> lbrace $$ indent (map toDoc ds) $$ rbrace
	toDoc (DVariable t n) = toDoc t <+> toDoc n
	toDoc (DMethod Nothing n ps ds) = text "method" <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$  indent (iterm semi (map toDoc ds)) $$ rbrace
	toDoc (DMethod (Just t) n ps ds) = text "method" <+> toDoc t <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (iterm semi (map toDoc ds)) $$ rbrace

instance PP Cmd where
	toDoc (CDecl t n) = toDoc t <+> toDoc n
	toDoc (CInvoke Nothing m ps) = toDoc m <> parens (cat $ isep (comma <> space) (map toDoc ps))
	toDoc (CInvoke (Just n) m ps) = toDoc n <+> text "<-" <+> toDoc m <> parens (cat $ isep (comma <> space) (map toDoc ps))

instance PP Expr where
	toDoc (EVariable n) = toDoc n
	toDoc (ELit l) = toDoc l


prettySyntax :: Module -> String
prettySyntax m = render (toDoc m)