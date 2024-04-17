{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$whiteSpace = [\ ]

tokens :-
  $whiteSpace+                  ;
  \t+                           ;
  [\n \;]+                      { \s -> TokenNewLine}
  if                            { \s -> TokenIf }
  else                          { \s -> TokenElse }
  while                         { \s -> TokenWhile}
  WriteLn                       { \s -> TokenWriteLn}
  ReadLn                        { \s -> TokenReadLn }
  \<                            { \s -> TokenLess }
  \>                            { \s -> TokenGreater }
  \<=                           { \s -> TokenLessEqual }
  \>=                           { \s -> TokenGreaterEqual }
  \=                            { \s -> TokenAssign }
  \==                           { \s -> TokenCompare }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenMultiply }
  \/                            { \s -> TokenDivide }
  \(                            { \s -> TokenOpenParenthesis }
  \)                            { \s -> TokenCloseParenthesis }
  \{                            { \s -> TokenOpenKey }
  \}                            { \s -> TokenCloseKey }
  \"                            { \s -> TokenQuote}
  
  $digit+                       { \s -> TokenInt (read s) }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenInt Int
           | TokenSym String
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenMultiply
           | TokenDivide
           | TokenNewLine
           | SpaceToken String
           | TokenOr
           | TokenAnd
           | TokenLess
           | TokenGreater
           | TokenLessEqual
           | TokenGreaterEqual
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenOpenParenthesis
           | TokenCloseParenthesis
           | TokenOpenKey
           | TokenCloseKey
           | TokenWriteLn
           | TokenReadLn
           | TokenCompare
           | TokenQuote
           deriving (Eq,Show)

scanTokens = alexScanTokens

}