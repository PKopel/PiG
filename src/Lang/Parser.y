{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lang.Parser 
        ( parseFile
        , parseProg
        ) 
where

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
    '~'             { Token _ TNot }
    '&&'            { Token _ TAnd }
    '||'            { Token _ TOr }
    ')'             { Token _ TRParen }
    '('             { Token _ TLParen }
    '}'             { Token _ TRBrace }
    '{'             { Token _ TLBrace }
    ']'             { Token _ TRBracket }
    '['             { Token _ TLBracket }
    ','             { Token _ TComma }
    ';'             { Token _ TSemi }
    eof             { Token _ TEOF }
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

File    : InSeq                 { $1 }
        | ExprList              { Seq $1 } 

Expr    : Atom                          { $1 }
        | ListLit                       { $1 }   
        | If                            { $1 }
        | '(' Expr ')'                  { $2 }
        | VAR '(' Expr ')' '=' Expr     { Assign $1 $3 $6 }
        | VAR '=' Expr                  { Assign $1 (Val Null) $3 }
        | while Expr do InSeq           { While $2 $4 }
        | while Expr do Expr            { While $2 $4 }
        | VAR FunAppl                   { FunApp $1 $2}
        | Expr '+' Expr                 { Binary ((+) :: Double -> Double -> Double) $1 $3 }
        | Expr '-' Expr                 { Binary ((-) :: Double -> Double -> Double) $1 $3 }
        | Expr '*' Expr                 { Binary ((*) :: Double -> Double -> Double) $1 $3 }
        | Expr '/' Expr                 { Binary ((/) :: Double -> Double -> Double) $1 $3 }
        | Expr '^' Expr                 { Binary ((**) :: Double -> Double -> Double) $1 $3 }
        | '-' Expr                      { Unary (negate :: Double -> Double) $2 }
        | '~' Expr                      { Unary not $2 }
        | Expr '&&' Expr                { Binary (&&) $1 $3 }
        | Expr '||' Expr                { Binary (||) $1 $3 }
        | Expr '==' Expr                { Binary ((==) :: Val -> Val -> Bool) $1 $3 }
        | Expr '!=' Expr                { Binary ((/=) :: Val -> Val -> Bool) $1 $3 }
        | Expr '<' Expr                 { Binary ((<) :: Val -> Val -> Bool) $1 $3 }
        | Expr '>' Expr                 { Binary ((>) :: Val -> Val -> Bool) $1 $3 }
        | '-<' Expr                     { Unary (-<) $2}
        | '>-' Expr                     { Unary (>-) $2}
        | Expr '<>' Expr                { Binary ((<>) :: Seq.Seq Val -> Seq.Seq Val -> Seq.Seq Val) $1 $3 }
        | Expr '><' Expr                { Binary ((<>) :: String -> String -> String) $1 $3 }

If      : if IfList                     { If $2 (Val Null) }
        | if IfList else InSeq          { If $2 $4 }
        | if IfList else Expr           { If $2 $4 }

IfList  : Expr do InSeq elif IfList     { ($1,$3):$5 }
        | Expr do Expr elif IfList      { ($1,$3):$5 }
        | Expr do InSeq                 { [($1,$3)] }
        | Expr do Expr                  { [($1,$3)] }

InSeq   : '{' ExprList '}'      { Seq $2 } 
        | '{' '}'               { Seq [] }      

ExprList : Expr ';' ExprList    { $1:$3 }
         | Expr ';'             { [$1] }
         | Expr                 { [$1] }

ListLit : '[' List ']'      { ListLiteral $2 }
        | '[' ']'           { ListLiteral [] }    

FunAppl : '(' List ')'      { $2 }
        | '(' Expr ')'      { [$2] }
        | '(' ')'           { [] }

List    : Expr ',' List     { $1:$3 }
        | Expr              { [$1] }

Atom    : VAR               { Var $1 }
        | Val               { Val $1 }

FunVal  : '(' ArgList ')'   { $2 }
        | '(' ')'           { [] }

ArgList : VAR ',' ArgList   { $1:$3 }
        | VAR               { [$1] }

Val     : true              { BoolVal True }
        | false             { BoolVal False }
        | null              { Null }
        | NUM               { AlgVal $1 }
        | CHAR              { CharVal $1 }
        | STR               { StrVal $1 }
        | FunVal '=>' InSeq { FunVal $1 $3 }                  

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseFile :: FilePath -> String -> Either String Expr
parseFile = runAlex' pig

parseProg :: String -> Either String Expr
parseProg s = runAlex s pig

}