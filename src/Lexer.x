{
module Lexer where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
@ident = $alpha[$alpha$digit\_]*
@type  = @ident"_t"
@integer = $digit+
@string = \"$alpha*\"

tokens :-

	$white+				;
	"module"			{ \p s -> TModule p }
	"class"				{ \p s -> TClass p }
	"implementing"			{ \p s -> TImplementing p }
	"method"			{ \p s -> TMethod p }
	@type				{ \p s -> TType p s }		-- NOTE: Order between @type and @ident!
	@ident				{ \p s -> TIdent p s }
	@integer			{ \p s -> TInteger p s }
	@string				{ \p s -> TString p s }
	","				{ \p s -> TComma  p}
	";"				{ \p s -> TSemiColon p }
	"("				{ \p s -> TLeftParen p }
	")"				{ \p s -> TRightParen p }
	"{"				{ \p s -> TLeftBrace p }
	"}"				{ \p s -> TRightBrace p }
	"="				{ \p s -> TAssign p }
	"<-"				{ \p s -> TInvoke p }

{
data Token =
	TModule AlexPosn		|
	TClass AlexPosn			|
	TImplementing AlexPosn		|
	TMethod AlexPosn		|
	TIdent AlexPosn String		|
	TType AlexPosn String		|
	TInteger AlexPosn String	|
	TString AlexPosn String		|
	TComma AlexPosn			|
	TSemiColon AlexPosn		|
	TLeftParen AlexPosn		|
	TRightParen AlexPosn		|
	TLeftBrace AlexPosn		|
	TRightBrace AlexPosn		|
	TAssign AlexPosn		|
	TInvoke AlexPosn
	deriving (Eq, Show)

tokenPosn (TModule p) = p
tokenPosn (TClass p) = p
tokenPosn (TImplementing p) = p
tokenPosn (TMethod p) = p
tokenPosn (TIdent p _) = p
tokenPosn (TType p _) = p
tokenPosn (TInteger p _) = p
tokenPosn (TString p _) = p
tokenPosn (TComma p) = p
tokenPosn (TSemiColon p) = p
tokenPosn (TLeftParen p) = p
tokenPosn (TRightParen p) = p
tokenPosn (TLeftBrace p) = p
tokenPosn (TRightBrace p) = p
tokenPosn (TInvoke p) = p
--tokenPosn t = error ("tokenPosn not implemented for " ++ (show t))

}
