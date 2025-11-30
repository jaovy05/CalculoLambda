{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*'
%left "&&" "||" '^'

%token 
    num         { TokenNum $$ }
    true        { TokenTrue }
    false       { TokenFalse }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenTimes }
    "&&"        { TokenAnd }
    "||"        { TokenOr }
    '^'         { TokenXor }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    '\\'        { TokenLambda }
    arrow       { TokenArrow }
    var         { TokenVar $$ }
    ','         {TokenVirgula}
    int         { TokenInt}
    bool        { TokenBool }
    ':'         { TokenDoisPontos }
%%

Exp     : App '+' App               { Add $1 $3 }
        | App '-' App               { Sub $1 $3 }
        | App '*' App               { Times $1 $3 }
        | App "&&" App              { And $1 $3 }
        | App "||" App              { Or $1 $3 }
        | App '^' App               { Xor $1 $3 }
        | if App then App else App  { If $2 $4 $6 }
        | App               { $1 }

App     : Exp atom                  { App $1 $2 }
        | atom                      { $1 }

atom    : num                       { Num $1 }
        | true                      { BTrue }
        | false                     { BFalse }
        | '(' Exp ',' ListVar       {Tuple ($2 : $4) }
        | var                       { Var $1}
        | '(' Exp ')'               { Paren $2 }
        | '\\' var ':' Ty arrow Exp { Lam $2 $4 $6}
        
        
ListVar : Exp ',' ListVar     {$1 : $3}
        | Exp ')'           {[$1]}

Ty      : int                 { TInt }
        | bool                { TBool }


{ 
parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}