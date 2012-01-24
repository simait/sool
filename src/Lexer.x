{
module Lexer where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
@ident = $alpha[$alpha$digit\_]*
@type  = @ident"_t"

tokens :-

	$white+				;
	"module"			{ \p s -> TModule p }
	"object"			{ \p s -> TObject p }
	"implementing"			{ \p s -> TImplementing p }
	"method"			{ \p s -> TMethod p }
	@type				{ \p s -> TType p s }		-- NOTE: Order between @type and @ident!
	@ident				{ \p s -> TIdent p s }
	","				{ \p s -> TComma  p}
	";"				{ \p s -> TSemiColon p }
	"("				{ \p s -> TLeftParen p }
	")"				{ \p s -> TRightParen p }
	"{"				{ \p s -> TLeftBrace p }
	"}"				{ \p s -> TRightBrace p }

{
data Token =
	TModule AlexPosn	|
	TObject AlexPosn	|
	TImplementing AlexPosn	|
	TMethod AlexPosn	|
	TIdent AlexPosn String	|
	TType AlexPosn String	|
	TComma AlexPosn		|
	TSemiColon AlexPosn	|
	TLeftParen AlexPosn	|
	TRightParen AlexPosn	|
	TLeftBrace AlexPosn	|
	TRightBrace AlexPosn
	deriving (Eq, Show)

tokenPosn (TModule p) = p
tokenPosn (TObject p) = p
tokenPosn (TImplementing p) = p
tokenPosn (TMethod p) = p
tokenPosn (TIdent p _) = p
tokenPosn (TType p _) = p
tokenPosn (TComma p) = p
tokenPosn (TSemiColon p) = p
tokenPosn (TLeftParen p) = p
tokenPosn (TRightParen p) = p
tokenPosn (TLeftBrace p) = p
tokenPosn (TRightBrace p) = p
--tokenPosn t = error ("tokenPosn not implemented for " ++ (show t))

}
