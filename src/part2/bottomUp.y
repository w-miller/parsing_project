{
module BottomUp where

import Tokeniser
}


%name bottomUp
%tokentype { Lexeme }
%error { parseError }

%token
    '+'    { LexPlus }
    '-'    { LexMinus }
    '*'    { LexMul }
    '/'    { LexDiv }
    '('    { LexLParen }
    ')'    { LexRParen }
    int    { LexInt $$ }
    id     { LexId $$ }

%%

E   : E '+' T       { Plus $1 $3 }
    | E '-' T       { Minus $1 $3 }
    | T             { T $1 }

T   : T '*' F       { Mul $1 $3 }
    | T '/' F       { Div $1 $3 }
    | F             { F $1 }

F   : '(' E ')'     { Parens $2 }
    | int           { Int $1 }
    | id            { Id $1 }

{
parseError :: [Lexeme] -> a
parseError _ = error "Parse error"

data E = Plus E T
       | Minus E T
       | T T
       deriving Show

data T = Mul T F
       | Div T F
       | F F
       deriving Show

data F = Parens E
       | Int Int
       | Id String
       deriving Show

main = getContents >>= print . bottomUp . tokenise
}
