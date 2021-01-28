{
module Lang.Parser where

import Lang.Tokens
import qualified Lang.Lexer as L
import Control.Monad.Error
import qualified Data.Sequence                 as Seq
}

%name pig
%lexer{L.lexer}{TEOF}
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
    '!='            { TNeq }
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
    true            { TTrue }
    false           { TFalse }
    null            { TNull }
    NUM             { TNum $$ }
    CHAR            { TChar $$ }
    STR             { TStr $$ }
    VAR             { TSym $$}

%%

Expr    : Atom                          { $1 }
        | ListLit                       { $1 }   
        | If                            { $1 }
        | '(' Expr ')'                  { $2 }
        | VAR '(' Atom ')' '=' Expr     { Assign $1 $3 $6 }
        | while Expr do Expr            { While $2 $4 }
        | read                          { Read }

ListLit : '[' List ']'      { ListLiteral $2 }
        | '[' ']'           { ListLiteral [] }    

Seq     : '{' List '}'      { Seq $2 }
        | '{' '}'           { Seq [] }

Appl    : '(' List ')'      { $2 }
        | '(' ')'           { [] }

List    : Expr ',' List     { $2:$4 }
        | Expr              { [$2] }

Atom    : VAR               { Var $1 }
        | Val               { Val $1 }

Val     : true              { BoolVal True }
        | false             { BoolVal False }
        | null              { Null }
        | NUM               { AlgVal $1 }
        | CHAR              { CharVal $1 }
        | STR               { StrVal $1 }
        | Appl '=>' Expr    { FunVal $1 $3 }
        | ListVal           { $1 }

ListVal : '[' ListVal       { ListVal (Seq.fromList $2) }
        | Val ',' ListVal   { $1:$3 }
        | Val ']'           { [$1] }
        | ']'               { [] }                    

{
parseError _ = throwError '!Parse Error'

}