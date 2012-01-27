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
	DClass Name [Decl] (Maybe Type) [Decl]		|
	DVariable Type Name				|
	DFunction Type Name [Decl] [Cmd]		|
	DMethod (Maybe Type) Name [Decl] [Cmd]
	deriving (Show,Eq)

data Cmd =
	CDecl Decl					|
	CAssign Name Expr				|
	CExpr Expr					|
	CInvoke (Maybe Name) Name [Expr]		|
	CReturn Expr
	deriving (Show, Eq)

data Expr =
	ELit Lit					|
	EVariable Name					|
	EBind Name Expr					|
	EApp Expr [Expr]
	deriving (Show, Eq)


instance PP Module where
	toDoc (Module n os) = text "module" <+> toDoc n <+> lbrace $$ indent (map toDoc os) $$ rbrace

instance PP Decl where
	toDoc (DClass n ps Nothing ds) = text "class" <+> toDoc n <> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (map toDoc ds) $$ rbrace
	toDoc (DClass n ps (Just t) ds) = text "class" <+> toDoc n <> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> text "implementing" <+> toDoc t <+> lbrace $$ indent (map toDoc ds) $$ rbrace
	toDoc (DVariable t n) = toDoc t <+> toDoc n
	toDoc (DFunction t n ps cs) = text "function" <+> toDoc t <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (iterm semi (map toDoc cs)) $$ rbrace
	toDoc (DMethod Nothing n ps cs) = text "method" <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$  indent (iterm semi (map toDoc cs)) $$ rbrace
	toDoc (DMethod (Just t) n ps cs) = text "method" <+> toDoc t <+> toDoc n <+> parens (cat $ isep (comma <> space) (map toDoc ps)) <+> lbrace $$ indent (iterm semi (map toDoc cs)) $$ rbrace

instance PP Cmd where
	toDoc (CDecl d) = toDoc d
	toDoc (CAssign n e) = toDoc n <+> text ":=" <+> toDoc e
	toDoc (CExpr e) = toDoc e
	toDoc (CInvoke Nothing m ps) = toDoc m <> parens (cat $ isep (comma <> space) (map toDoc ps))
	toDoc (CInvoke (Just n) m ps) = toDoc n <+> text ":=" <+> toDoc m <> parens (cat $ isep (comma <> space) (map toDoc ps))
	toDoc (CReturn e) = text "return" <+> toDoc e
	--toDoc a = error (show a)

instance PP Expr where
	toDoc (EVariable n) = toDoc n
	toDoc (ELit l) = toDoc l
	toDoc (EBind n e) = toDoc n <+> text "="  <+> toDoc e
	toDoc (EApp e es) = toDoc e <> parens (cat $ isep (comma <> space) (map toDoc es))


prettySyntax :: Module -> String
prettySyntax m = render (toDoc m)