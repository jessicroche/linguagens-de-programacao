{
module Parser where
import Lexer 
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
    num     { TokNum $$ }
    '+'     { TokSum }
    '*'     { TokMult }
    '('     { TokOpenPar }
    ')'     { TokClosePar }
    '-'     { TokSub }
    '/'     { TokDiv }
    '='     { TokEqual }

%left '+'
%left '-'
%left '*'
%left '/'
%left '='

%% 

Exp : num               { Num $1 }
    | Exp '+' Exp       { Sum $1 $3 }
    | Exp '*' Exp       { Mult $1 $3 }
    | Exp '-' Exp       { Sub $1 $3 }
    | Exp '/' Exp       { Div $1 $3 }
    | Exp '=' Exp       { Equal $1 $3}
    | '(' Exp ')'       { $2 }

{ 
parseError :: [Token] -> a 
parseError _ = error "Erro sintatico!"
}
