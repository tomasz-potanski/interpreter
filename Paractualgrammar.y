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
 '(' { PT _ (TS _ 1) }
 ')' { PT _ (TS _ 2) }
 '*' { PT _ (TS _ 3) }
 '+' { PT _ (TS _ 4) }
 ',' { PT _ (TS _ 5) }
 '-' { PT _ (TS _ 6) }
 '.' { PT _ (TS _ 7) }
 '/' { PT _ (TS _ 8) }
 ':' { PT _ (TS _ 9) }
 ';' { PT _ (TS _ 10) }
 '=' { PT _ (TS _ 11) }
 'Boolean' { PT _ (TS _ 12) }
 'Integer' { PT _ (TS _ 13) }
 'begin' { PT _ (TS _ 14) }
 'const' { PT _ (TS _ 15) }
 'end' { PT _ (TS _ 16) }
 'program' { PT _ (TS _ 17) }
 'var' { PT _ (TS _ 18) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Program :: { Program }
Program : ProgramNameHeader Block '.' { Programm $1 $2 } 


ProgramNameHeader :: { ProgramNameHeader }
ProgramNameHeader : 'program' Ident ';' { ProgNameHeaderNotBlank $2 } 
  | {- empty -} { ProgNameHeaderBlank }


Block :: { Block }
Block : ConstantDeclaration VariableDeclaration 'begin' ListStmt 'end' { Blockk $1 $2 (reverse $4) } 


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | ListStmt Stmt { flip (:) $1 $2 }


VariableDeclaration :: { VariableDeclaration }
VariableDeclaration : 'var' ListVarDeclarationLine { VBExists $2 } 
  | {- empty -} { VBDoesntExists }


VarDeclarationLine :: { VarDeclarationLine }
VarDeclarationLine : ListIdent ':' Type ';' { DLList $1 $3 } 


ListVarDeclarationLine :: { [VarDeclarationLine] }
ListVarDeclarationLine : VarDeclarationLine { (:[]) $1 } 
  | VarDeclarationLine ListVarDeclarationLine { (:) $1 $2 }


ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } 
  | Ident ',' ListIdent { (:) $1 $3 }


ConstantDeclaration :: { ConstantDeclaration }
ConstantDeclaration : {- empty -} { ConstDeclBlank } 
  | 'const' ListConstDeclLine { ConstDeclNotBlank $2 }


ConstDeclLine :: { ConstDeclLine }
ConstDeclLine : Ident '=' LiteralValue ';' { ConsDeclLine $1 $3 } 


LiteralValue :: { LiteralValue }
LiteralValue : Integer { LiteralValInt $1 } 


ListConstDeclLine :: { [ConstDeclLine] }
ListConstDeclLine : ConstDeclLine { (:[]) $1 } 
  | ConstDeclLine ListConstDeclLine { (:) $1 $2 }


Stmt :: { Stmt }
Stmt : Exp { Stmtt $1 } 


Exp :: { Exp }
Exp : Exp '+' Exp1 { EAdd $1 $3 } 
  | Exp '-' Exp1 { ESub $1 $3 }
  | Exp1 { $1 }


Exp1 :: { Exp }
Exp1 : Exp1 '*' Exp2 { EMul $1 $3 } 
  | Exp1 '/' Exp2 { EDiv $1 $3 }
  | Exp2 { $1 }


Exp2 :: { Exp }
Exp2 : Integer { EInt $1 } 
  | '(' Exp ')' { $2 }


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

