{
module Parser where
import Lexer 
import Name
import Type
import qualified Syntax
}

%name mdl
%tokentype {Token}
%error {parseError}

%token
	"module"			{ TModule _ }
	"object"			{ TObject _ }
	"implementing"			{ TImplementing _ }
	"method"			{ TMethod _ }
	ident				{ TIdent _ $$ }
	type				{ TType _ $$ }
	","				{ TComma _ }
	";"				{ TSemiColon _ }
	"("				{ TLeftParen _ }
	")"				{ TRightParen _ }
	"{"				{ TLeftBrace _ }
	"}"				{ TRightBrace _ }

%%

Module :
	"module" ident "{" ObjectDeclList "}"	{ Module $2 $4 }

ObjectDeclList :
	"object" ident "{" VarDeclList MethodDeclList "}" ObjectDeclList			{ Just (Object $2 Nothing $4 $5 $7) }
	|"object" ident "implementing" type "{" VarDeclList MethodDeclList "}" ObjectDeclList	{ Just (Object $2 (Just $4) $6 $7 $9) }
	|											{ Nothing }

TypeDecl :
	type ident			{ Decl $1 $2 }

ParamDeclList :
	TypeDecl			{ Just (DeclList $1 Nothing) }
	| TypeDecl ParamDeclListTail	{ Just (DeclList $1 $2) }
	|				{ Nothing }

ParamDeclListTail :
	"," TypeDecl ParamDeclList	{ Just (DeclList $2 $3) }

MethodDeclList :
	MethodDecl MethodDeclList	{ Just (MethodDeclList $1 $2) }
	|				{ Nothing }

MethodDecl :
	"method" ident "(" ParamDeclList ")" "{" VarDeclList "}"	{ Method $2 Nothing $4 $7 }
	| "method" type ident "(" ParamDeclList ")" "{" VarDeclList "}"	{ Method $3 (Just $2) $5 $8 }

VarDeclList :
	TypeDecl ";" VarDeclList	{ Just (DeclList $1 $3) }
	|				{ Nothing }

{

parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error at line " ++ (show line) ++ " column " ++ (show column) ++ ".")
	where
		AlexPn _ line column = tokenPosn x

data Module = Module String (Maybe Object)
	deriving (Eq, Show)

data Object = Object String (Maybe String) (Maybe DeclList) (Maybe MethodDeclList) (Maybe Object)
	deriving (Eq, Show)

data Decl = Decl String String
	deriving (Eq, Show)

data DeclList = DeclList Decl (Maybe DeclList)
	deriving (Eq, Show)

data MethodDeclList = MethodDeclList Method (Maybe MethodDeclList)
	deriving (Eq, Show)

data Method = Method String (Maybe String) (Maybe DeclList) (Maybe DeclList)
	deriving (Eq, Show)

toSyntax (Module s mo) = Syntax.Module (mkName s) (collectObjs mo)
	where
		collectObjs Nothing = []
		collectObjs (Just (Object n mi mds mmds mo)) = Syntax.Object (mkName n) (toType mi) ((collectDecls mds) ++ (collectMethodDecls mmds)):(collectObjs mo)
		collectDecls Nothing = []
		collectDecls (Just (DeclList (Decl t n) mds)) = Syntax.Decl (Type t) (Name n):collectDecls mds
		collectMethodDecls Nothing = []
		collectMethodDecls (Just (MethodDeclList (Method n mr mps mds) mmd)) = Syntax.MethodDecl (toType mr) (mkName n) (collectDecls mps) (collectDecls mds):collectMethodDecls mmd
		toType Nothing = Nothing
		toType (Just t) = Just (mkType t)
}
