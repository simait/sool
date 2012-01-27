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
	"function"			{ \p s -> TFunction p }
	"return"			{ \p s -> TReturn p }
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
	":="				{ \p s -> TAssign p }
	"="				{ \p s -> TEq p }
	"+"				{ \p s -> TAdd p }
	"-"				{ \p s -> TSub p }
	"*"				{ \p s -> TMul p }
	"/"				{ \p s -> TDiv p }
	"<"				{ \p s -> TLt p }
	">"				{ \p s -> TGt p }

{
data Token =
	TModule AlexPosn		|
	TClass AlexPosn			|
	TImplementing AlexPosn		|
	TMethod AlexPosn		|
	TFunction AlexPosn		|
	TReturn AlexPosn		|
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
	TEq AlexPosn			|
	TAdd AlexPosn			|
	TSub AlexPosn			|
	TMul AlexPosn			|
	TDiv AlexPosn			|
	TLt AlexPosn			|
	TGt AlexPosn
	deriving (Eq, Show)

tokenPosn (TModule p) = p
tokenPosn (TClass p) = p
tokenPosn (TImplementing p) = p
tokenPosn (TMethod p) = p
tokenPosn (TFunction p) = p
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
tokenPosn (TAssign p) = p
tokenPosn (TEq p) = p
tokenPosn (TAdd p) = p
tokenPosn (TSub p) = p
tokenPosn (TMul p) = p
tokenPosn (TDiv p) = p
tokenPosn (TLt p) = p
tokenPosn (TGt p) = p
--tokenPosn t = error ("tokenPosn not implemented for " ++ (show t))

}
