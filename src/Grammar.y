{
module Grammar where
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  TokenIf          { TokenIf }
  TokenElse        { TokenElse }
  TokenWhile       { TokenWhile }
  TokenWriteLn     { TokenWriteLn }
  TokenReadLn      { TokenReadLn }
  TokenLess        { TokenLess }
  TokenGreater     { TokenGreater }
  TokenLessEqual   { TokenLessEqual }
  TokenGreaterEqual{ TokenGreaterEqual }
  TokenAssign      { TokenAssign }
  TokenCompare     { TokenCompare }
  TokenPlus        { TokenPlus }
  TokenMinus       { TokenMinus }
  TokenMultiply    { TokenMultiply }
  TokenDivide      { TokenDivide }
  TokenOpenParenthesis { TokenOpenParenthesis }
  TokenCloseParenthesis { TokenCloseParenthesis }
  TokenOpenKey     { TokenOpenKey }
  TokenCloseKey    { TokenCloseKey }
  TokenQuote       { TokenQuote }
  TokenInt     { TokenInt $$ }
  TokenSym  { TokenSym $$ }
  TokenNewLine     { TokenNewLine }
  TokenSemiColon    { TokenSemiColon }
  TokenString       { TokenString $$}
  TokenOr            { TokenOr }
  TokenAnd          { TokenAnd}


%left '+' '-'
%left '*'
%left '<' '>' '<=' '>='

%%

prog : stmt prog              { $1 : $2 } 
     | stmt                   { [$1] }
     | TokenNewLine prog      { $2 }
     | TokenNewLine           { [] }

stmt : assign TokenSemiColon                           { AssignStmt $1 }
     | print TokenSemiColon                            { PrintStmt $1 }
     | while_loop TokenNewLine                         { WhileStmt $1 }
     | assign TokenSemiColon TokenNewLine              { AssignStmt $1 }
     | print TokenSemiColon TokenNewLine               { PrintStmt $1}
     | if_exp TokenNewLine                             { IfStmt $1 }

assign : TokenSym TokenAssign exp          { Assign $1 $3 }

print : TokenWriteLn TokenOpenParenthesis exp TokenCloseParenthesis   { Print $3 }

while_loop : TokenWhile TokenOpenParenthesis cond TokenCloseParenthesis TokenOpenKey prog TokenCloseKey   { WhileLoop $3 $6 }

if_exp : TokenIf TokenOpenParenthesis cond TokenCloseParenthesis TokenOpenKey prog TokenCloseKey { IfExp $3 $6}

exp : TokenSym                     { SymExp $1 }
    | TokenInt                     { IntExp $1 }
    | exp TokenPlus exp            { PlusExp $1 $3 } 
    | exp TokenMinus exp           { MinusExp $1 $3 }
    | exp TokenMultiply exp        { MultiplyExp $1 $3 }
    | exp TokenDivide exp          { DivideExp $1 $3 }
    | TokenString                  { StringExp $1}

cond : exp TokenLess exp             { LessCond $1 $3 }
     | exp TokenLessEqual exp        { LessEqualCond $1 $3 }
     | exp TokenGreater exp          { GreaterCond $1 $3 }
     | exp TokenGreaterEqual exp     { GreaterEqualCond $1 $3 }
     | exp TokenCompare exp          { EqualCond $1 $3}
     | cond TokenAnd cond            { AndCond $1 $3 }
     | cond TokenOr cond             { OrCond $1 $3 }

{
parseError :: [Token] -> a
parseError tokens = error ("¡Compilation Failed! Head Token: " ++ show (head tokens))



data Stmt = AssignStmt Assign
          | PrintStmt Print
          | WhileStmt WhileLoop
          | IfStmt IfExp
          deriving (Eq, Show)

data Assign = Assign String Exp
            deriving (Eq, Show)

data Print = Print Exp
           deriving (Eq, Show)

data WhileLoop = WhileLoop Cond [Stmt]
               deriving (Eq, Show)

data IfExp = IfExp Cond [Stmt]
               deriving (Eq, Show)

data Exp = SymExp String
         | IntExp Int
         | PlusExp Exp Exp
         | MinusExp Exp Exp
         | MultiplyExp Exp Exp
         | DivideExp Exp Exp
         | StringExp String
         deriving (Eq, Show)

         

data Cond = LessCond Exp Exp
          | LessEqualCond Exp Exp
          | GreaterCond Exp Exp
          | GreaterEqualCond Exp Exp
          | EqualCond Exp Exp
          | AndCond Cond Cond
          | OrCond Cond Cond

          deriving (Eq, Show)
}
