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

%token
	"module"			{ TModule _ }
	"class"				{ TClass _ }
	"implementing"			{ TImplementing _ }
	"method"			{ TMethod _ }
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
	"="				{ TAssign _ }
	"<-"				{ TInvoke _}

%%

Module :
	"module" ident "{" ObjectDeclList "}"	{ Module $2 $4 }

ObjectDeclList :
	"class" ident "(" ParamDeclList ")" "{" VarDeclList MethodDeclList "}" ObjectDeclList				{ Just (Class $2 $4 Nothing $7 $8 $10) }
	|"class" ident "(" ParamDeclList ")" "implementing" type "{" VarDeclList MethodDeclList "}" ObjectDeclList	{ Just (Class $2 $4 (Just $7) $9 $10 $12) }
	|														{ Nothing }

TypeDecl :
	type ident							{ Decl $1 $2 }

ParamDeclList :
	TypeDecl ParamDeclListTail					{ Just (DeclList $1 $2) }
	|								{ Nothing }

ParamDeclListTail :
	"," TypeDecl ParamDeclListTail					{ Just (DeclList $2 $3) }
	|								{ Nothing }

MethodDeclList :
	MethodDecl MethodDeclList					{ Just (MethodDeclList $1 $2) }
	|								{ Nothing }

MethodDecl :
	"method" ident "(" ParamDeclList ")" "{" CmdList "}"		{ Method Nothing $2 $4 $7 }
	| "method" type ident "(" ParamDeclList ")" "{" CmdList "}"	{ Method (Just $2) $3 $5 $8 }

VarDeclList :
	TypeDecl ";" VarDeclList					{ Just (DeclList $1 $3) }
	|								{ Nothing }

CmdList :
	type ident ";" CmdList						{ Just (CDecl $1 $2 $4) }
	| ident "(" ExprList ")" ";" CmdList				{ Just (CInvoke Nothing $1 $3 $6) }
	| ident "=" ident "(" ExprList ")" ";" CmdList			{ Just (CInvoke (Just $1) $3 $5 $8) }
	| ident "=" Expr ";" CmdList					{ Just (CAssign $1 $3 $5) }
	|								{ Nothing }

Expr :
	integer 							{ ELit (LInt (read $1)) }
	| string							{ ELit (LString $1) }
	| ident								{ EVariable $1 }
	
ExprList :
	Expr ExprListTail						{ Just (ExprList $1 $2) }
	|								{ Nothing }
ExprListTail:
	"," Expr ExprListTail						{ Just (ExprList $2 $3) }
	|								{ Nothing }

{

parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error at line " ++ (show line) ++ " column " ++ (show column) ++ ".")
	where
		AlexPn _ line column = tokenPosn x

data Module = Module String (Maybe Class)
	deriving (Show, Eq)

data Class = Class String (Maybe DeclList) (Maybe String) (Maybe DeclList) (Maybe MethodDeclList) (Maybe Class)
	deriving (Show, Eq)

data Decl = Decl String String
	deriving (Show, Eq)

data CmdList =
	CDecl String String (Maybe CmdList) 				|
	CAssign String Expr (Maybe CmdList)				|
	CInvoke (Maybe String) String (Maybe ExprList) (Maybe CmdList)
	deriving (Show, Eq)

data Expr =
	ELit Lit	|
	EVariable String
	deriving (Show, Eq)

data ExprList = ExprList Expr (Maybe ExprList)
	deriving (Show, Eq)

data ParamList = ParamList String (Maybe ParamList)
	deriving (Show, Eq)

data DeclList = DeclList Decl (Maybe DeclList)
	deriving (Show, Eq)

data MethodDeclList = MethodDeclList Method (Maybe MethodDeclList)
	deriving (Show, Eq)

data Method = Method (Maybe String) String (Maybe DeclList) (Maybe CmdList)
	deriving (Show, Eq)

toSyntax (Module s mo) = Syntax.Module (mkName s) (collectObjs mo)
	where
		collectObjs Nothing = []
		collectObjs (Just (Class n mps mi mds mmds mo)) = Syntax.DClass (mkName n) (collectDecls mps) (toType mi) ((collectDecls mds) ++ (collectMethodDecls mmds)):(collectObjs mo)

		collectCmds Nothing = []
		collectCmds (Just c) = collectCmds' c
		collectCmds' (CDecl t n mc) = Syntax.CDecl (mkType t) (mkName n):collectCmds mc
		collectCmds' (CInvoke lhs m mps mc) = Syntax.CInvoke (toName lhs) (mkName m) (collectExprs mps):collectCmds mc

		collectExprs Nothing = []
		collectExprs (Just (ExprList (ELit l) mvs)) = (Syntax.ELit l):collectExprs mvs
		collectExprs (Just (ExprList (EVariable n) mvs)) = (Syntax.EVariable (mkName n)):collectExprs mvs

		collectDecls Nothing = []
		collectDecls (Just (DeclList (Decl t n) mds)) = Syntax.DVariable (mkType t) (mkName n):collectDecls mds

		collectMethodDecls Nothing = []
		collectMethodDecls (Just (MethodDeclList (Method mr n mps mcs) mmd)) =
			Syntax.DMethod (toType mr) (mkName n) (collectDecls mps) (collectCmds mcs):collectMethodDecls mmd

		toName Nothing = Nothing
		toName (Just n) = Just (mkName n)

		toType Nothing = Nothing
		toType (Just t) = Just (mkType t)

parseMDL :: String -> Syntax.Module
parseMDL = toSyntax . mdl . alexScanTokens
}
