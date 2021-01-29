{
module Lang.Lexer where

import Lang.Tokens
import Control.Monad.Except
}

%wrapper "basic"

$digit      = [0-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
$alpha      = [a-zA-Z]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white
$eol        = [\n]

@comment_line      = "//" ( [^\n\/]* [^\n]* )?
@comment_start     = "/*"
@comment_end       = "*/"

@escape            = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
@decimal_suffix    = \. [0-9][0-9_]*
@double            = [0-9][0-9_]* @decimal_suffix?
@string            = \" ($graphic # \")* \"
@id                = [A-Za-z][A-Za-z'_]*
@char              = \'($graphic # $special) | @escape\'

tokens :-
    $white+			;
    "if"            { const TIf }
    "elif"          { const TElIf }
    "else"          { const TElse }
    "while"         { const TWhile }
    "do"            { const TDo }
    "print"         { const TPrint }
    "read"          { const TRead }
    "exit"|":exit"  { const TExit }
    ":help"         { const THelp }
    ":rm"           { const TRM }
    ":clear"        { const TClear }
    ":load"         { const TLoad }
    "+"             { const TPlus }
    "-"             { const TMinus }
    "*"             { const TStar }
    "/"             { const TSlash }
    "=>"            { const TFatArr }
    "="             { const TAssign }
    "^"             { const TDash }
    "<"             { const TLt }
    ">"             { const TGt }
    "=="            { const TEq }
    "!="            { const TNEq }
    "<>"            { const TLtGt }
    "><"            { const TGtLt }
    "-<"            { const TRFork }
    ">-"            { const TLFork }
    "&&"            { const TAnd }
    "||"            { const TOr }
    ")"             { const TRParen }
    "("             { const TRParen }
    "}"             { const TRBrace }
    "{"             { const TLBrace }
    "]"             { const TRBracket }
    "["             { const TLBracket }
    ","             { const TComma }
    ";"             { const TSemi }
    "true"          { const TTrue }
    "false"         { const TFalse }
    "null"          { const TNull }
    @double         { TNum . read }
    @char           { TChar . head . drop 1 . init }
    @string         { TStr . drop 1 . init }
    @id             { TSym}
    @comment_line.* ;
    @comment_start(.*\n)*@comment_end ;

{

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)

}


