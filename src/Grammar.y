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


%left '+' '-'
%left '*'
%left '<' '>' '<=' '>='

%%

prog : stmt prog              { $1 : $2 } 
     | stmt                   { [$1] }

stmt : assign TokenNewLine              { AssignStmt $1 }
     | print TokenNewLine               { PrintStmt $1 }
     | while_loop TokenNewLine          { WhileStmt $1 }

assign : TokenSym TokenAssign exp          { Assign $1 $3 }

print : TokenWriteLn TokenOpenParenthesis exp TokenCloseParenthesis   { Print $3 }

while_loop : TokenWhile TokenOpenParenthesis cond TokenCloseParenthesis TokenOpenKey prog TokenCloseKey   { WhileLoop $3 $6 }

exp : TokenSym                     { SymExp $1 }
    | TokenInt                     { IntExp $1 }
    | exp TokenPlus exp            { PlusExp $1 $3 } 
    | exp TokenMinus exp           { MinusExp $1 $3 }
    | exp TokenMultiply exp        { MultiplyExp $1 $3 }
    | exp TokenDivide exp          { DivideExp $1 $3 }

cond : exp TokenLess exp             { LessCond $1 $3 }
     | exp TokenLessEqual exp        { LessEqualCond $1 $3 }
     | exp TokenGreater exp          { GreaterCond $1 $3 }
     | exp TokenGreaterEqual exp     { GreaterEqualCond $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Stmt = AssignStmt Assign
          | PrintStmt Print
          | WhileStmt WhileLoop
          deriving (Eq, Show)

data Assign = Assign String Exp
            deriving (Eq, Show)

data Print = Print Exp
           deriving (Eq, Show)

data WhileLoop = WhileLoop Cond [Stmt]
               deriving (Eq, Show)

data Exp = SymExp String
         | IntExp Int
         | PlusExp Exp Exp
         | MinusExp Exp Exp
         | MultiplyExp Exp Exp
         | DivideExp Exp Exp
         deriving (Eq, Show)

data Cond = LessCond Exp Exp
          | LessEqualCond Exp Exp
          | GreaterCond Exp Exp
          | GreaterEqualCond Exp Exp
          deriving (Eq, Show)
}
