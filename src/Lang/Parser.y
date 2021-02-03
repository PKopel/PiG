{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lang.Parser where

import Lang.Tokens
import Lang.Lexer
import Control.Monad.Except
import qualified Data.Sequence as Seq
import Import               
}
%name pig
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TEOF }
-- Without this we get a type error
%error { happyError }

%token
    if              { Token _ TIf }
    elif            { Token _ TElIf }
    else            { Token _ TElse }
    while           { Token _ TWhile }
    do              { Token _ TDo }
    print           { Token _ TPrint }
    read            { Token _ TRead }
    exit            { Token _ TExit }
    help            { Token _ THelp }
    rm              { Token _ TRM }
    clear           { Token _ TClear }
    load            { Token _ TLoad }
    '+'             { Token _ TPlus }
    '-'             { Token _ TMinus }
    '*'             { Token _ TStar }
    '/'             { Token _ TSlash }
    '=>'            { Token _ TFatArr }
    '='             { Token _ TAssign }
    '^'             { Token _ TDash }
    '<'             { Token _ TLt }
    '>'             { Token _ TGt }
    '=='            { Token _ TEq }
    '!='            { Token _ TNEq }
    '<>'            { Token _ TLtGt }
    '><'            { Token _ TGtLt }
    '-<'            { Token _ TRFork }
    '>-'            { Token _ TLFork }
    '&&'            { Token _ TAnd }
    '||'            { Token _ TOr }
    ')'             { Token _ TRParen }
    '('             { Token _ TRParen }
    '}'             { Token _ TRBrace }
    '{'             { Token _ TLBrace }
    ']'             { Token _ TRBracket }
    '['             { Token _ TLBracket }
    ','             { Token _ TComma }
    ';'             { Token _ TSemi }
    true            { Token _ TTrue }
    false           { Token _ TFalse }
    null            { Token _ TNull }
    NUM             { Token _ (TNum $$) }
    CHAR            { Token _ (TChar $$) }
    STR             { Token _ (TStr $$) }
    VAR             { Token _ (TSym $$) }


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
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseFile :: FilePath -> String -> Either String Prog
parseFile = runAlex' parse

parseProg :: String -> Either String Prog
parseProg s = runAlex s parse

}