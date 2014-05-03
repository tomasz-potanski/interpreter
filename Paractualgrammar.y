-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Paractualgrammar where
import Absactualgrammar
import Lexactualgrammar
import ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '.' { PT _ (TS _ 1) }
 '=' { PT _ (TS _ 2) }
 'begin' { PT _ (TS _ 3) }
 'end' { PT _ (TS _ 4) }

L_ident  { PT _ (TV $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Program :: { Program }
Program : Block '.' { Programm $1 } 


Block :: { Block }
Block : 'begin' ListStmt 'end' { Blockk (reverse $2) } 


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | ListStmt Stmt { flip (:) $1 $2 }


Stmt :: { Stmt }
Stmt : Exp { Stmtt $1 } 


Exp :: { Exp }
Exp : Ident '=' Exp { EAss $1 $3 } 
  | Ident '=' Exp { EAss $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

