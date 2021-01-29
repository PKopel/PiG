{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lang.Parser where

import Lang.Tokens
import qualified Lang.Lexer as L
import Control.Monad.Except
import qualified Data.Sequence as Seq
import Import               
}

%name pig
%monad { Except String } { (>>=) } { return }
%tokentype{Token}
%error{parseError}

%token
    if              { TIf }
    elif            { TElIf }
    else            { TElse }
    while           { TWhile }
    do              { TDo }
    print           { TPrint }
    read            { TRead }
    exit            { TExit }
    help            { THelp }
    rm              { TRM }
    clear           { TClear }
    load            { TLoad }
    '+'             { TPlus }
    '-'             { TMinus }
    '*'             { TStar }
    '/'             { TSlash }
    '=>'            { TFatArr }
    '='             { TAssign }
    '^'             { TDash }
    '<'             { TLt }
    '>'             { TGt }
    '=='            { TEq }
    '!='            { TNEq }
    '<>'            { TLtGt }
    '><'            { TGtLt }
    '-<'            { TRFork }
    '>-'            { TLFork }
    '&&'            { TAnd }
    '||'            { TOr }
    ')'             { TRParen }
    '('             { TRParen }
    '}'             { TRBrace }
    '{'             { TLBrace }
    ']'             { TRBracket }
    '['             { TLBracket }
    ','             { TComma }
    ';'             { TSemi }
    true            { TTrue }
    false           { TFalse }
    null            { TNull }
    NUM             { TNum $$ }
    CHAR            { TChar $$ }
    STR             { TStr $$ }
    VAR             { TSym $$}


%left '&&' '||'
%left '+' '-'
%left '*' '/'
%left '^'
%left '<>' '><'
%%

Prog    : Drct          { Drct $1 }
        | Expr          { Stmt $1 }

Drct    : exit          { Exit }
        | help          { Help }
        | clear         { Clear }
        | load STR      { Load $2 }
        | rm VAR        { Rm $2 }

Expr    : Atom                          { $1 }
        | ListLit                       { $1 }   
        | If                            { $1 }
        | Seq                           { $1 }
        | '(' Expr ')'                  { $2 }
        | VAR '(' Atom ')' '=' Expr     { Assign $1 $3 $6 }
        | while Expr do Expr            { While $2 $4 }
        | read                          { Read }
        | print Appl                    { Print $2 }
        | Expr '+' Expr                 { Binary ((+) :: Double -> Double -> Double) $1 $3 }
        | Expr '-' Expr                 { Binary ((-) :: Double -> Double -> Double) $1 $3 }
        | Expr '*' Expr                 { Binary ((*) :: Double -> Double -> Double) $1 $3 }
        | Expr '/' Expr                 { Binary ((/) :: Double -> Double -> Double) $1 $3 }
        | Expr '^' Expr                 { Binary ((**) :: Double -> Double -> Double) $1 $3 }
        | '-' Expr                      { Unary opposite $2 }
        | Expr '&&' Expr                { Binary (&&) $1 $3 }
        | Expr '||' Expr                { Binary (||) $1 $3 }
        | Expr '==' Expr                { Binary (==) $1 $3 }
        | Expr '!=' Expr                { Binary (/=) $1 $3 }
        | Expr '<' Expr                 { Binary (<) $1 $3 }
        | Expr '>' Expr                 { Binary (>) $1 $3 }
        | '-<' Expr                     { Unary (-<) $2}
        | '>-' Expr                     { Unary (>-) $2}
        | Expr '<>' Expr                { Binary ((<>) :: Seq.Seq Val -> Seq.Seq Val -> Seq.Seq Val) $1 $3 }
        | Expr '><' Expr                { Binary ((<>) :: String -> String -> String) $1 $3 }

If      : if Expr do Expr               { If [($2,$4)] (Val Null) }
        | if IfList else Expr           { If $2 $4 }

IfList  : Expr do Expr elif IfList      { ($1,$3):$5 }
        | Expr do Expr                  { [($1,$3)] }

Seq     : '{' Seq           { Seq $2 }
        | Expr ';' Seq      { $1:$3  }    
        | '}'               { [] }

ListLit : '[' List ']'      { ListLiteral $2 }
        | '[' ']'           { ListLiteral [] }    

Appl    : '(' List ')'      { $2 }
        | '(' ')'           { [] }

List    : Expr ',' List     { $1:$3 }
        | Expr              { [$1] }

Atom    : VAR               { Var $1 }
        | Val               { Val $1 }

Val     : true              { BoolVal True }
        | false             { BoolVal False }
        | null              { Null }
        | NUM               { AlgVal $1 }
        | CHAR              { CharVal $1 }
        | STR               { StrVal $1 }
        | Appl '=>' Expr    { FunVal $1 $3 }                  

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseProg :: String -> Either String Prog
parseProg input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    

}