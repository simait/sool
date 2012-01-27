{
module Parser where
import Lexer 
import Name
import Type
import Util
import qualified Syntax
}

%name mdl
%tokentype {Token}
%error {parseError}

%left "="
%nonassoc "<" ">"
%left "+" "-"
%left "*" "/"
%left NEG

%token
	"module"			{ TModule _ }
	"class"				{ TClass _ }
	"implementing"			{ TImplementing _ }
	"method"			{ TMethod _ }
	"function"			{ TFunction _ }
	"return"			{ TReturn _ }
	ident				{ TIdent _ $$ }
	type				{ TType _ $$ }
	integer				{ TInteger _ $$ }
	string				{ TString _ $$ }
	","				{ TComma _ }
	";"				{ TSemiColon _ }
	"("				{ TLeftParen _ }
	")"				{ TRightParen _ }
	"{"				{ TLeftBrace _ }
	"}"				{ TRightBrace _ }
	":="				{ TAssign _ }
	"="				{ TEq _ }
	"+"				{ TAdd _ }
	"-"				{ TSub _ }
	"*"				{ TMul _ }
	"/"				{ TDiv _ }
	"<"				{ TGt _ }
	">"				{ TLt _ }

%%

Module :
	"module" ident "{" Classes "}"	{ Module $2 $4 }

Classes :
	"class" ident "(" Params ")" "{" ClassBody "}" Classes				{ Just (Class $2 $4 Nothing $7 $9) }
	|"class" ident "(" Params ")" "implementing" type "{" ClassBody "}" Classes	{ Just (Class $2 $4 (Just $7) $9 $11) }
	|										{ Nothing }

ClassBody :
	type ident ";" ClassBody							{ Just (CVariableDecl $1 $2 $4) }
	| "function" type ident "(" Params ")" "{" Cmds "}" ClassBody			{ Just (CFunctionDecl $2 $3 $5 $8 $10) }
	| "method" ident "(" Params ")" "{" Cmds "}" ClassBody				{ Just (CMethodDecl Nothing $2 $4 $7 $9) }
	| "method" type ident "(" Params ")" "{" Cmds "}" ClassBody			{ Just (CMethodDecl (Just $2) $3 $5 $8 $10) }
	|										{ Nothing }

Cmds :
	type ident ";" Cmds								{ Just (CVariableDecl $1 $2 $4) }
	| "function" type ident "(" Params ")" "{" Cmds "}" Cmds			{ Just (CFunctionDecl $2 $3 $5 $8 $10) }
	| ident "(" Exprs ")" ";" Cmds							{ Just (CInvoke Nothing $1 $3 $6) }
	| ident ":=" ident "(" Exprs ")" ";" Cmds					{ Just (CInvoke (Just $1) $3 $5 $8) }
	| ident ":=" Expr ";" Cmds							{ Just (CAssign $1 $3 $5) }
	| Expr ";" Cmds									{ Just (CExpr $1 $3) }
	| "return" Expr ";" Cmds								{ Just (CReturn $2 $4) }
	|										{ Nothing }

Params :
	type ident ParamsTail								{ Just (DeclList (Decl $1 $2) $3) }
	|										{ Nothing }

ParamsTail :
	"," type ident ParamsTail							{ Just (DeclList (Decl $2 $3) $4) }
	|										{ Nothing }

Exprs :
	Expr ExprsTail									{ Just (ExprList $1 $2) }
	|										{ Nothing }
ExprsTail:
	"," Expr ExprsTail								{ Just (ExprList $2 $3) }
	|										{ Nothing }

Expr :
	Expr "+" Expr									{ EApp "+" [$1, $3] }
	| Expr "-" Expr									{ EApp "-" [$1, $3] }
	| Expr "*" Expr									{ EApp "*" [$1, $3] }
	| Expr "/" Expr									{ EApp "/" [$1, $3] }
	| Expr "<" Expr									{ EApp "<" [$1, $3] }
	| Expr ">" Expr									{ EApp ">" [$1, $3] }
	| "-" Expr %prec NEG								{ EApp "-" [$2] }
	| ident "=" Expr								{ EBind $1 $3 }
	| "(" Expr ")"									{ $2 }
	| integer 									{ ELit (LInt (read $1)) }
	| string									{ ELit (LString $1) }
	| ident										{ EVariable $1 }

{

parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error at line " ++ (show line) ++ " column " ++ (show column) ++ ".")
	where
		AlexPn _ line column = tokenPosn x

data Module = Module String (Maybe Class)
	deriving (Show, Eq)

data Class = Class String (Maybe DeclList) (Maybe String) (Maybe CmdList) (Maybe Class)
	deriving (Show, Eq)

data Decl = Decl String String
	deriving (Show, Eq)

data CmdList =
	CMethodDecl (Maybe String) String (Maybe DeclList) (Maybe CmdList) (Maybe CmdList)	|
	CFunctionDecl String String (Maybe DeclList) (Maybe CmdList) (Maybe CmdList)		|
	CVariableDecl String String (Maybe CmdList) 						|
	CAssign String Expr (Maybe CmdList)							|
	CExpr Expr (Maybe CmdList)								|
	CInvoke (Maybe String) String (Maybe ExprList) (Maybe CmdList)				|
	CReturn Expr (Maybe CmdList)
	deriving (Show, Eq)

data Expr =
	ELit Lit										|
	EVariable String									|
	EBind String Expr									|
	EApp String [Expr]
	deriving (Show, Eq)

data ExprList = ExprList Expr (Maybe ExprList)
	deriving (Show, Eq)

data ParamList = ParamList String (Maybe ParamList)
	deriving (Show, Eq)

data DeclList = DeclList Decl (Maybe DeclList)
	deriving (Show, Eq)

toSyntax (Module s mo) = Syntax.Module (mkName s) (collectClasses mo)
	where
		collectClasses Nothing = []
		collectClasses (Just (Class n mps mi mcs mc)) = Syntax.DClass (mkName n) (collectDecls mps) (toType mi) (collectClass mcs):collectClasses mc

		collectClass Nothing = []
		collectClass (Just c) = collectClass' c
		collectClass' (CVariableDecl t n mc) = Syntax.DVariable (mkType t) (mkName n):collectClass mc
		collectClass' (CMethodDecl mr n mps mcs mc) = Syntax.DMethod (toType mr) (mkName n) (collectDecls mps) (collectCmds mcs):collectClass mc
		collectClass' (CFunctionDecl r n mps mcs mc) = Syntax.DFunction (mkType r) (mkName n) (collectDecls mps) (collectCmds mcs):collectClass mc
		
		collectCmds Nothing = []
		collectCmds (Just c) = collectCmds' c
		collectCmds' (CVariableDecl t n mc) = Syntax.CDecl (Syntax.DVariable (mkType t) (mkName n)):collectCmds mc
		collectCmds' (CFunctionDecl t n mps mbs mc) = Syntax.CDecl (Syntax.DFunction (mkType t) (mkName n) (collectDecls mps) (collectCmds mbs)):collectCmds mc
		collectCmds' (CAssign n e mc) = Syntax.CAssign (mkName n) (convertExpr e):collectCmds mc
		collectCmds' (CInvoke lhs m mps mc) = Syntax.CInvoke (toName lhs) (mkName m) (collectExprs mps):collectCmds mc
		collectCmds' (CReturn e mc) = Syntax.CReturn (convertExpr e):collectCmds mc
		collectCmds' (CExpr e mc) = Syntax.CExpr (convertExpr e):collectCmds mc

		collectExprs Nothing = []
		collectExprs (Just (ExprList e mvs)) = convertExpr e:collectExprs mvs

		convertExpr (ELit l) = Syntax.ELit l
		convertExpr (EVariable n) = Syntax.EVariable (mkName n)
		convertExpr (EBind n e) = Syntax.EBind (mkName n) (convertExpr e)
		convertExpr (EApp n es) = Syntax.EApp (Syntax.EVariable (mkName n)) (map convertExpr es)

		collectDecls Nothing = []
		collectDecls (Just (DeclList (Decl t n) mds)) = Syntax.DVariable (mkType t) (mkName n):collectDecls mds

		toName Nothing = Nothing
		toName (Just n) = Just (mkName n)

		toType Nothing = Nothing
		toType (Just t) = Just (mkType t)

parseMDL :: String -> Syntax.Module
parseMDL = toSyntax . mdl . alexScanTokens
}
