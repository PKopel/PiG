{
{-# OPTIONS -w  #-}
module Lang.Lexer where


import Prelude hiding (lex)
import Control.Monad ( liftM )
import Lang.Tokens
}

%wrapper "monadUserState"

$digit      = [0-9]

@comment_line      = "//" ( [^\n\/]* [^\n]* )?
@comment_start     = "/*"
@comment_end       = "*/"

@escape            = \\ [nrt\\'"0] 
@decimal_suffix    = \. [0-9][0-9_]*
@double            = [0-9][0-9_]* @decimal_suffix?
@string            = \" ( @escape | $printable # \")* \"
@id                = [A-Za-z][A-Za-z0-9'_]*
@char              = \' ( @escape | [^\\'\n\t\r] ) \'
tokens :-
    $white+			;
    "if"              { lex' TIf }
    "elif"            { lex' TElIf }
    "else"            { lex' TElse }
    "while"           { lex' TWhile }
    "do"              { lex' TDo }
    "print"           { lex' TPrint }
    "read()"          { lex' TRead }
    "exit()"|":exit"  { lex' TExit }
    ":help"           { lex' THelp }
    ":rm"             { lex' TRM }
    ":clear"          { lex' TClear }
    ":load"           { lex' TLoad }
    "+"               { lex' TPlus }
    "-"               { lex' TMinus }
    "*"               { lex' TStar }
    "/"               { lex' TSlash }
    "=>"              { lex' TFatArr }
    "="               { lex' TAssign }
    "^"               { lex' TDash }
    "<"               { lex' TLt }
    ">"               { lex' TGt }
    "=="              { lex' TEq }
    "!="              { lex' TNEq }
    "<>"              { lex' TLtGt }
    "><"              { lex' TGtLt }
    "-<"              { lex' TRFork }
    ">-"              { lex' TLFork }
    "~"               { lex' TNot }
    "&&"              { lex' TAnd }
    "||"              { lex' TOr }
    ")"               { lex' TRParen }
    "("               { lex' TLParen }
    "}"               { lex' TRBrace }
    "{"               { lex' TLBrace }
    "]"               { lex' TRBracket }
    "["               { lex' TLBracket }
    ","               { lex' TComma }
    ";"               { lex' TSemi }
    "true"            { lex' TTrue }
    "false"           { lex' TFalse }
    "null"            { lex' TNull }
    @double           { lex (TNum . read) }
    @char             { lex lexChar }
    @string           { lex (lexString []) }
    @id               { lex TSym}
    @comment_line.*   ;
    @comment_start(.*\n)*@comment_end ;

{
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "PiG"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

data Token = Token AlexPosn TokenType
  deriving ( Show )

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TEOF

lex :: (String -> TokenType) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: TokenType -> AlexAction Token
lex' = lex . const

escape :: Char -> Char
escape c = case c of 
              'n' -> '\n'
              't' -> '\t'
              'r' -> '\r'
              '0' -> '\0'
              o -> o

lexString :: String -> String -> TokenType
lexString acc [] = TStr (reverse acc)
lexString acc ('"':s) = lexString acc s
lexString acc ('\\':c:s) = lexString (ec:acc) s
  where ec = escape c
lexString acc (c:s) = lexString (c:acc) s

lexChar :: String -> TokenType
lexChar ('\'':c:'\'':_) = TChar c
lexChar ('\'':'\\':c:'\'':_) = TChar (escape c)

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}


