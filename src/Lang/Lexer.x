{
{-# OPTIONS -w  #-}
module Lang.Lexer
  ( alexMonadScan'
  , alexError'
  , runAlex
  , runAlex'
  , Alex(..)
  , Token(..)
  )
where

import Control.Monad ( liftM )
import Lang.Tokens
}

%wrapper "monadUserState"

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
    "if"              { tok' TIf }
    "elif"            { tok' TElIf }
    "else"            { tok' TElse }
    "while"           { tok' TWhile }
    "do"              { tok' TDo }
    "print"           { tok' TPrint }
    "read()"          { tok' TRead }
    "exit()"|":exit"  { tok' TExit }
    ":help"           { tok' THelp }
    ":rm"             { tok' TRM }
    ":clear"          { tok' TClear }
    ":load"           { tok' TLoad }
    "+"               { tok' TPlus }
    "-"               { tok' TMinus }
    "*"               { tok' TStar }
    "/"               { tok' TSlash }
    "=>"              { tok' TFatArr }
    "="               { tok' TAssign }
    "^"               { tok' TDash }
    "<"               { tok' TLt }
    ">"               { tok' TGt }
    "=="              { tok' TEq }
    "!="              { tok' TNEq }
    "<>"              { tok' TLtGt }
    "><"              { tok' TGtLt }
    "-<"              { tok' TRFork }
    ">-"              { tok' TLFork }
    "~"               { tok' TNot }
    "&&"              { tok' TAnd }
    "||"              { tok' TOr }
    ")"               { tok' TRParen }
    "("               { tok' TLParen }
    "}"               { tok' TRBrace }
    "{"               { tok' TLBrace }
    "]"               { tok' TRBracket }
    "["               { tok' TLBracket }
    ","               { tok' TComma }
    ";"               { tok' TSemi }
    "true"            { tok' TTrue }
    "false"           { tok' TFalse }
    "null"            { tok' TNull }
    @double           { tok (TNum . read) }
    @char             { tok lexChar }
    @string           { tok (lexString []) }
    @id               { tok TSym}
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

tok :: (String -> TokenType) -> AlexAction Token
tok f = \(p,_,_,s) i -> return $ Token p (f (take i s))

tok' :: TokenType -> AlexAction Token
tok' = tok . const

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


