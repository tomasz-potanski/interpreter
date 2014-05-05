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
 '!=' { PT _ (TS _ 1) }
 '&&' { PT _ (TS _ 2) }
 '(' { PT _ (TS _ 3) }
 ')' { PT _ (TS _ 4) }
 '*' { PT _ (TS _ 5) }
 '*=' { PT _ (TS _ 6) }
 '+' { PT _ (TS _ 7) }
 '++' { PT _ (TS _ 8) }
 '+=' { PT _ (TS _ 9) }
 ',' { PT _ (TS _ 10) }
 '-' { PT _ (TS _ 11) }
 '--' { PT _ (TS _ 12) }
 '-=' { PT _ (TS _ 13) }
 '.' { PT _ (TS _ 14) }
 '..' { PT _ (TS _ 15) }
 '/' { PT _ (TS _ 16) }
 '/=' { PT _ (TS _ 17) }
 ':' { PT _ (TS _ 18) }
 ':=' { PT _ (TS _ 19) }
 ';' { PT _ (TS _ 20) }
 '<' { PT _ (TS _ 21) }
 '<=' { PT _ (TS _ 22) }
 '=' { PT _ (TS _ 23) }
 '==' { PT _ (TS _ 24) }
 '>' { PT _ (TS _ 25) }
 '>=' { PT _ (TS _ 26) }
 'Array' { PT _ (TS _ 27) }
 'Boolean' { PT _ (TS _ 28) }
 'Char' { PT _ (TS _ 29) }
 'Double' { PT _ (TS _ 30) }
 'False' { PT _ (TS _ 31) }
 'Integer' { PT _ (TS _ 32) }
 'String' { PT _ (TS _ 33) }
 'True' { PT _ (TS _ 34) }
 '[' { PT _ (TS _ 35) }
 ']' { PT _ (TS _ 36) }
 'begin' { PT _ (TS _ 37) }
 'const' { PT _ (TS _ 38) }
 'do' { PT _ (TS _ 39) }
 'elif' { PT _ (TS _ 40) }
 'else' { PT _ (TS _ 41) }
 'end' { PT _ (TS _ 42) }
 'for' { PT _ (TS _ 43) }
 'if' { PT _ (TS _ 44) }
 'of' { PT _ (TS _ 45) }
 'print' { PT _ (TS _ 46) }
 'program' { PT _ (TS _ 47) }
 'then' { PT _ (TS _ 48) }
 'to' { PT _ (TS _ 49) }
 'var' { PT _ (TS _ 50) }
 'while' { PT _ (TS _ 51) }
 '||' { PT _ (TS _ 52) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }
L_doubl  { PT _ (TD $$) }
L_charac { PT _ (TC $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }

Program :: { Program }
Program : ProgramNameHeader Block '.' { Programm $1 $2 } 


ProgramNameHeader :: { ProgramNameHeader }
ProgramNameHeader : 'program' Ident ';' { ProgNameHeaderNotBlank $2 } 
  | {- empty -} { ProgNameHeaderBlank }


Block :: { Block }
Block : VariableDeclaration Stmt2 { Blockk $1 $2 } 


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


BoolLit :: { BoolLit }
BoolLit : 'True' { BoolLitTrue } 
  | 'False' { BoolLitFalse }


Stmt2 :: { Stmt }
Stmt2 : 'begin' ListStmt 'end' { SBlock (reverse $2) } 
  | '(' Stmt ')' { $2 }


Stmt :: { Stmt }
Stmt : Ident ':=' Exp ';' { SAss $1 $3 } 
  | Ident '[' Integer ']' ':=' Exp ';' { SAssArray $1 $3 $6 }
  | Ident ':=' BExp ';' { SAssBool $1 $3 }
  | Ident '[' Integer ']' ':=' BExp ';' { SAssArrayBool $1 $3 $6 }
  | Ident '*=' Exp ';' { SAssMult $1 $3 }
  | Ident '/=' Exp ';' { SAssDiv $1 $3 }
  | Ident '+=' Exp ';' { SAssAdd $1 $3 }
  | Ident '-=' Exp ';' { SAssSub $1 $3 }
  | IfStmt { SIf $1 }
  | 'while' BExp 'do' Stmt { SWhile $2 $4 }
  | 'for' Ident ':=' Exp 'to' Exp 'do' Stmt { SFor $2 $4 $6 $8 }
  | 'print' Printable ';' { SPrint $2 }
  | Stmt1 { $1 }


Stmt1 :: { Stmt }
Stmt1 : '++' Ident ';' { SPreIncr $2 } 
  | '--' Ident ';' { SPreDecr $2 }
  | Stmt2 { $1 }


IfStmt :: { IfStmt }
IfStmt : 'if' BExp 'then' Stmt { SimpleIf $2 $4 } 
  | IfStmt1 { $1 }


IfStmt1 :: { IfStmt }
IfStmt1 : 'if' BExp 'then' Stmt 'else' Stmt { IfElse $2 $4 $6 } 
  | IfStmt2 { $1 }


IfStmt2 :: { IfStmt }
IfStmt2 : 'if' BExp 'then' Stmt 'elif' BExp 'then' Stmt { IfElif $2 $4 $6 $8 } 
  | IfStmt3 { $1 }


IfStmt3 :: { IfStmt }
IfStmt3 : 'if' BExp 'then' Stmt 'elif' BExp 'then' Stmt 'else' Stmt { IfElifElse $2 $4 $6 $8 $10 } 
  | '(' IfStmt ')' { $2 }


Printable1 :: { Printable }
Printable1 : Ident { SPId $1 } 
  | '(' Printable ')' { $2 }


Printable :: { Printable }
Printable : Printable1 { $1 } 


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
  | Ident { EId $1 }
  | Ident '[' Integer ']' { EArray $1 $3 }
  | '(' Exp ')' { $2 }


BExp :: { BExp }
BExp : BExp '||' BExp1 { BOr $1 $3 } 
  | BExp1 { $1 }


BExp1 :: { BExp }
BExp1 : BExp1 '&&' BExp2 { BAnd $1 $3 } 
  | BExp2 { $1 }


BExp2 :: { BExp }
BExp2 : Exp RelOp Exp { BRel $1 $2 $3 } 
  | BoolLit { BBLit $1 }
  | '(' BExp ')' { $2 }


RelOp :: { RelOp }
RelOp : '<' { LTH } 
  | '<=' { LE }
  | '>' { GTH }
  | '>=' { GE }
  | '==' { EQU }
  | '!=' { NE }


Type :: { Type }
Type : 'Integer' { TInt } 
  | 'Boolean' { TBool }
  | 'String' { TString }
  | 'Char' { TChar }
  | 'Double' { TDouble }
  | Type1 { $1 }


Type2 :: { Type }
Type2 : 'Array' '[' Integer '..' Integer ']' 'of' Type { TArray $3 $5 $8 } 
  | '(' Type ')' { $2 }


Type1 :: { Type }
Type1 : Type2 { $1 } 


LitVal :: { LitVal }
LitVal : String { LiteralValueString $1 } 
  | Double { LiteralValueDouble $1 }
  | Char { LiteralValueChar $1 }



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

