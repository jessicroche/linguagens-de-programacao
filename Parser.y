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
    '=='    { TokEqual }
    '!='    { TokDiff }
    '&&'    { TokAnd }
    '||'    { TokOr }
    '>'     { TokMaior }
    '>='    { TokMaiorIgual}
    '<'     { TokMenor }
    '<='    { TokMenorIgual }
    '!'     { TokNot }
    'if'    { TokIf }
    'then'  { TokThen }
    'else'  { TokElse }
    'true'  { TokTrue }
    'false' { TokFalse }
    var     { TokVar $$ }
    'lam'   { TokLam }
    'let'   { TokLet }


%left '||'
%left '&&'
%left '=='
%left '!='
%left '>'
%left '<'
%left '>='
%left '<='
%left '+'
%left '-'
%left '*'
%left '/'
%left '!'

%% 

Exp : num               { Num $1 }
    | Exp '+' Exp       { Sum $1 $3 }
    | Exp '*' Exp       { Mult $1 $3 }
    | Exp '-' Exp       { Sub $1 $3 }
    | Exp '/' Exp       { Div $1 $3 }
    | Exp '==' Exp       { Equal $1 $3 }
    | '(' Exp ')'       { $2 }
    | Exp '&&' Exp      { And $1 $3 }
    | '!' Exp            { Not $2 }
    | Exp '||' Exp      { Or $1 $3 }
    | Exp '!=' Exp      { Diff $1 $3 }
    | Exp '>' Exp       { Maior $1 $3 }
    | Exp '<' Exp       { Menor $1 $3 }
    | Exp '>=' Exp      { MaiorIgual $1 $3 }
    | Exp '<=' Exp      { MenorIgual $1 $3 }
    | 'if' Exp 'then' Exp 'else' Exp {If $2 $4 $6}
    | 'true'  { Boolean True }
    | 'false' { Boolean  False }
    | var { Var $1 }
    

{ 
parseError :: [Token] -> a 
parseError _ = error "Erro sintatico!"
}
