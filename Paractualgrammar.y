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
 ',' { PT _ (TS _ 1) }
 '.' { PT _ (TS _ 2) }
 ':' { PT _ (TS _ 3) }
 ';' { PT _ (TS _ 4) }
 '=' { PT _ (TS _ 5) }
 'Boolean' { PT _ (TS _ 6) }
 'Integer' { PT _ (TS _ 7) }
 'Var' { PT _ (TS _ 8) }
 'begin' { PT _ (TS _ 9) }
 'end' { PT _ (TS _ 10) }

L_ident  { PT _ (TV $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Program :: { Program }
Program : Block '.' { Programm $1 } 


Block :: { Block }
Block : VariableDeclaration Block2 { Blockk $1 $2 } 


Block2 :: { Block }
Block2 : 'begin' ListStmt 'end' { Blockk2 (reverse $2) } 


VariableDeclaration :: { VariableDeclaration }
VariableDeclaration : 'Var' DeclarationLines { VBExists $2 } 
  | {- empty -} { VBDoesntExists }


DeclarationLines :: { DeclarationLines }
DeclarationLines : ListIdent ':' Type { DLList $1 $3 } 
  | Ident ':' Type { DLSingle $1 $3 }


ListIdent :: { [Ident] }
ListIdent : Ident ';' { (:[]) $1 } 
  | Ident ';' ListIdent { (:) $1 $3 }
  | {- empty -} { [] }
  | Ident { (:[]) $1 }
  | Ident ',' ListIdent { (:) $1 $3 }


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | ListStmt Stmt { flip (:) $1 $2 }


Stmt :: { Stmt }
Stmt : Exp { Stmtt $1 } 


Exp :: { Exp }
Exp : Ident '=' Exp { EAss $1 $3 } 


Type :: { Type }
Type : 'Integer' { TInt } 
  | 'Boolean' { TBool }



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

