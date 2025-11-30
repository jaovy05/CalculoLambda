{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*'

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }

%% 

Exp     : num           { Num $1 }
        | true          { BTrue }
        | false         { BFalse }
        | Exp '+' Exp   { Add $1 $3 }
        | Exp '*' Exp   { Times $1 $3 }
        | Exp "&&" Exp  { And $1 $3 }
        | Exp "||" Exp  { Or $1 $3 }
        | '(' Exp ')'   { Paren $2 }

{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}