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
    ','         { TokenVirgula }
    int         { TokenInt}
    bool        { TokenBool }
    ':'         { TokenDoisPontos }
    '.'         { TokenDot }
%%

Exp     : Exp '+' Exp               { Add $1 $3 }
        | Exp '-' Exp               { Sub $1 $3 }
        | Exp '*' Exp               { Times $1 $3 }
        | Exp "&&" Exp              { And $1 $3 }
        | Exp "||" Exp              { Or $1 $3 }
        | Exp '^' Exp               { Xor $1 $3 }
        | if Exp then Exp else Exp  { If $2 $4 $6 }
        | App               { $1 }

App     : App atom                  { App $1 $2 }
        | atom                      { $1 }
        | App '.' num              { Proj $1 $3 }

atom    : num                       { Num $1 }
        | true                      { BTrue }
        | false                     { BFalse }
        | '(' Exp ',' ListVar ')'   { Tuple ($2 : $4) }
        | var                       { Var $1}
        | '(' Exp ')'               { Paren $2 }
        | '\\' var ':' Ty arrow Exp { Lam $2 $4 $6}
        
ListVar : Exp ',' ListVar   {$1 : $3}
        | Exp               {[$1]}

Ty      : int                     { TInt }
        | bool                    { TBool }
        | '(' Exp ',' TyList ')'  { TTuple ($2 : $4) }

TyList  : Ty ',' TyList       {$1 : $3}
        | Ty                  {[$1]}

{ 
parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}