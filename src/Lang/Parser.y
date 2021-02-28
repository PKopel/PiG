{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser 
        ( parseFile
        , parseProg
        ) 
where

import Lang.Tokens
import Lang.Lexer
import Control.Monad.Except
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as Lazy
import Utils.Types   
import Utils.Util           
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
    load            { Token _ TLoad }
    '+'             { Token _ TPlus }
    '-'             { Token _ TMinus }
    '*'             { Token _ TStar }
    '/'             { Token _ TSlash }
    '%'             { Token _ TMod }
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
    true            { Token _ TTrue }
    false           { Token _ TFalse }
    null            { Token _ TNull }
    NUM             { Token _ (TNum $$) }
    CHAR            { Token _ (TChar $$) }
    STR             { Token _ (TStr $$) }
    VAR             { Token _ (TSym $$) }


%left '==' '!='
%left '&&' '||'
%left '<' '>' '%'
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
        | load Expr                     { Load $2 }
        | VAR '(' Expr ')' '=' Expr     { Assign $1 $3 $6 }
        | VAR '=' Expr                  { Assign $1 (Val Null) $3 }
        | while Expr do InSeq           { While $2 $4 }
        | while Expr do Expr            { While $2 $4 }
        | VAR FunAppl                   { FunApp $1 $2}
        | Expr '+' Expr                 { FunApp "add" [$1,$3] }
        | Expr '-' Expr                 { FunApp "sub" [$1,$3] }
        | Expr '*' Expr                 { FunApp "mul" [$1,$3] }
        | Expr '/' Expr                 { FunApp "div" [$1,$3] }
        | Expr '^' Expr                 { FunApp "pow" [$1,$3] }
        | Expr '%' Expr                 { FunApp "mod" [$1,$3] }
        | '-' Expr                      { FunApp "neg" [$2] }
        | '~' Expr                      { FunApp "not" [$2] }
        | Expr '&&' Expr                { FunApp "and" [$1,$3] }
        | Expr '||' Expr                { FunApp "or" [$1,$3] }
        | Expr '==' Expr                { FunApp "eq" [$1,$3] }
        | Expr '!=' Expr                { FunApp "neq" [$1,$3] }
        | Expr '<' Expr                 { FunApp "lt" [$1,$3] }
        | Expr '>' Expr                 { FunApp "gt" [$1,$3] }
        | '-<' Expr                     { FunApp "lst" [$2] }
        | '>-' Expr                     { FunApp "fst" [$2] }
        | Expr '<>' Expr                { FunApp "catList" [$1,$3] }
        | Expr '><' Expr                { FunApp "catStr" [$1,$3] }

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
        | STR               { StrVal $ Lazy.unpack $1 }
        | FunVal '=>' InSeq { FunVal $1 $3 }                  

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError p ("parse error at token '" <> Lazy.pack (show t) <> "'")

parseFile :: FilePath -> Lazy.Text -> Either Lazy.Text Expr
parseFile = runAlex' pig

parseProg :: Lazy.Text -> Either Lazy.Text Expr
parseProg s = runAlex s pig

}