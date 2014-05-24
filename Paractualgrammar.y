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
 '.equals' { PT _ (TS _ 16) }
 '/' { PT _ (TS _ 17) }
 '/=' { PT _ (TS _ 18) }
 ':' { PT _ (TS _ 19) }
 ':=' { PT _ (TS _ 20) }
 ';' { PT _ (TS _ 21) }
 '<' { PT _ (TS _ 22) }
 '<-' { PT _ (TS _ 23) }
 '<--' { PT _ (TS _ 24) }
 '<=' { PT _ (TS _ 25) }
 '=' { PT _ (TS _ 26) }
 '==' { PT _ (TS _ 27) }
 '>' { PT _ (TS _ 28) }
 '>=' { PT _ (TS _ 29) }
 'Array' { PT _ (TS _ 30) }
 'Boolean' { PT _ (TS _ 31) }
 'Char' { PT _ (TS _ 32) }
 'Double' { PT _ (TS _ 33) }
 'False' { PT _ (TS _ 34) }
 'Function' { PT _ (TS _ 35) }
 'Integer' { PT _ (TS _ 36) }
 'RecEnd' { PT _ (TS _ 37) }
 'Record' { PT _ (TS _ 38) }
 'String' { PT _ (TS _ 39) }
 'True' { PT _ (TS _ 40) }
 'Void' { PT _ (TS _ 41) }
 '[' { PT _ (TS _ 42) }
 ']' { PT _ (TS _ 43) }
 'begin' { PT _ (TS _ 44) }
 'const' { PT _ (TS _ 45) }
 'do' { PT _ (TS _ 46) }
 'elif' { PT _ (TS _ 47) }
 'else' { PT _ (TS _ 48) }
 'end' { PT _ (TS _ 49) }
 'endif' { PT _ (TS _ 50) }
 'for' { PT _ (TS _ 51) }
 'function' { PT _ (TS _ 52) }
 'if' { PT _ (TS _ 53) }
 'intToStr' { PT _ (TS _ 54) }
 'of' { PT _ (TS _ 55) }
 'print' { PT _ (TS _ 56) }
 'proc' { PT _ (TS _ 57) }
 'procedure' { PT _ (TS _ 58) }
 'program' { PT _ (TS _ 59) }
 'ref' { PT _ (TS _ 60) }
 'return' { PT _ (TS _ 61) }
 'strToInt' { PT _ (TS _ 62) }
 'then' { PT _ (TS _ 63) }
 'to' { PT _ (TS _ 64) }
 'var' { PT _ (TS _ 65) }
 'while' { PT _ (TS _ 66) }
 '||' { PT _ (TS _ 67) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }
L_charac { PT _ (TC $$) }
L_doubl  { PT _ (TD $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }

Program :: { Program }
Program : ProgramNameHeader Block '.' { Programm $1 $2 } 


ProgramNameHeader :: { ProgramNameHeader }
ProgramNameHeader : 'program' Ident ';' { ProgNameHeaderNotBlank $2 } 
  | {- empty -} { ProgNameHeaderBlank }


Block :: { Block }
Block : VariableDeclaration ProcDeclaration Stmt2 { Blockk $1 $2 $3 } 


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | ListStmt Stmt { flip (:) $1 $2 }


ProcDeclaration :: { ProcDeclaration }
ProcDeclaration : 'proc' ListProcDeclLine { PExists (reverse $2) } 
  | 'proc' ListProcDeclLine ListFuncDeclLine { PFExists (reverse $2) (reverse $3) }
  | 'proc' ListFuncDeclLine { FExists (reverse $2) }
  | {- empty -} { PDoesntExist }


ProcDeclLine :: { ProcDeclLine }
ProcDeclLine : 'procedure' Ident '(' ')' ';' VariableDeclaration Stmt2 { PLineNonArg $2 $6 $7 } 
  | 'procedure' Ident '(' VarDeclarationLine ')' ';' VariableDeclaration Stmt2 { PLineArg $2 $4 $7 $8 }


ListProcDeclLine :: { [ProcDeclLine] }
ListProcDeclLine : {- empty -} { [] } 
  | ListProcDeclLine ProcDeclLine { flip (:) $1 $2 }


FuncDeclLine :: { FuncDeclLine }
FuncDeclLine : 'function' Ident '(' ')' ':' Type ';' VariableDeclaration Stmt2 { FLineNonArg $2 $6 $8 $9 } 
  | 'function' Ident '(' VarDeclarationLine ')' ':' Type ';' VariableDeclaration Stmt2 { FLineArg $2 $4 $7 $9 $10 }


ListFuncDeclLine :: { [FuncDeclLine] }
ListFuncDeclLine : {- empty -} { [] } 
  | ListFuncDeclLine FuncDeclLine { flip (:) $1 $2 }


FuncArg :: { FuncArg }
FuncArg : VarDeclarationLine { NonEmptyArgs $1 } 
  | {- empty -} { EmptyArgs }


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


Stmt :: { Stmt }
Stmt : ';' { SBlank } 
  | Ident ':=' Ident ';' { SAttr $1 $3 }
  | Ident '[' Integer ']' ':=' Ident ';' { SAttrArray $1 $3 $6 }
  | Ident ':=' Exp ';' { SAss $1 $3 }
  | Ident '[' Integer ']' ':=' Exp ';' { SAssArray $1 $3 $6 }
  | Ident '.' Ident ':=' Exp ';' { SAssRec $1 $3 $5 }
  | Ident ':=' BExp ';' { SAssBool $1 $3 }
  | Ident ':=' BoolLit ';' { SAssBoolLit $1 $3 }
  | 'return' Integer ';' ';' { SReturn $2 }
  | Ident ':=' String ';' { SAssString $1 $3 }
  | Ident ':=' 'strToInt' String ';' { SAssStrToInt $1 $4 }
  | Ident ':=' 'intToStr' Integer ';' { SAzs $1 $4 }
  | Ident '[' Integer ']' ':=' BExp ';' { SAssArrayBool $1 $3 $6 }
  | Ident '[' Integer ']' ':=' BoolLit ';' { SAssArrayBoolLit $1 $3 $6 }
  | Ident '[' Integer ']' ':=' String ';' { SAssArrayString $1 $3 $6 }
  | Ident '*=' Exp ';' { SAssMult $1 $3 }
  | Ident '/=' Exp ';' { SAssDiv $1 $3 }
  | Ident '+=' Exp ';' { SAssAdd $1 $3 }
  | Ident '-=' Exp ';' { SAssSub $1 $3 }
  | IfStmt { SIf $1 }
  | 'while' BExp 'do' Stmt { SWhile $2 $4 }
  | 'for' Ident ':=' Exp 'to' Exp 'do' Stmt { SFor $2 $4 $6 $8 }
  | 'print' '<-' '(' String ')' ';' { SPrintString $4 }
  | 'print' '<-' '(' BoolLit ')' ';' { SPrintBLit $4 }
  | 'print' '<--' '(' Ident ')' ';' { SPrintId $4 }
  | 'print' '<--' '(' Ident '[' Integer ']' ')' ';' { SPrintArray $4 $6 }
  | 'print' '<-' '(' Exp ')' ';' { SPrintExp $4 }
  | 'print' '<-' '(' BExp ')' ';' { SPrintBExp $4 }
  | 'print' '<-' '(' Char ')' ';' { SPrintCharLit $4 }
  | 'print' '<--' '(' Ident '.' Ident ')' ';' { SPrintIdRec $4 $6 }
  | 'print' '<--' '(' Ident '(' ')' ')' ';' { SPrintFun $4 }
  | 'print' '<--' '(' Ident '(' Exp ')' ')' ';' { SPrintFunExp $4 $6 }
  | 'print' '<--' '(' Ident '(' BExp ')' ')' ';' { SPrintFunBExp $4 $6 }
  | 'print' '<--' '(' Ident '(' String ')' ')' ';' { SPrintFunString $4 $6 }
  | 'print' '<--' '(' Ident '(' Ident ')' ')' ';' { SPrintFunId $4 $6 }
  | 'print' '<--' '(' Ident '(' Ident '[' Integer ']' ')' ')' ';' { SPrintFunIdArray $4 $6 $8 }
  | Ident ':=' Procc { SProcAttr $1 $3 }
  | Ident '(' ')' ';' { SProcCall $1 }
  | Ident '(' Ident ')' ';' { SProcCallId $1 $3 }
  | Ident '(' Ident '[' Integer ']' ')' ';' { SProcCallIdArray $1 $3 $5 }
  | Ident '(' Exp ')' ';' { SProcCallExp $1 $3 }
  | Ident '(' BExp ')' ';' { SProcCallBExp $1 $3 }
  | Ident '(' String ')' ';' { SProcCallString $1 $3 }
  | Ident '(' FuncDeclLine ')' ';' { SProcCallFuncSyg $1 $3 }
  | Ident '(' 'ref' Ident ')' ';' { SProcCallIdRef $1 $4 }
  | Stmt1 { $1 }


Stmt2 :: { Stmt }
Stmt2 : 'begin' ListStmt 'end' { SBlock (reverse $2) } 
  | '(' Stmt ')' { $2 }


Stmt1 :: { Stmt }
Stmt1 : '++' Ident ';' { SPreIncr $2 } 
  | '--' Ident ';' { SPreDecr $2 }
  | Stmt2 { $1 }


IfStmt :: { IfStmt }
IfStmt : 'if' BExp 'then' Stmt 'endif' { SimpleIf $2 $4 } 
  | IfStmt1 { $1 }


IfStmt1 :: { IfStmt }
IfStmt1 : 'if' BExp 'then' Stmt 'else' Stmt 'endif' { IfElse $2 $4 $6 } 
  | IfStmt2 { $1 }


IfStmt2 :: { IfStmt }
IfStmt2 : 'if' BExp 'then' Stmt 'elif' BExp 'then' Stmt 'endif' { IfElif $2 $4 $6 $8 } 
  | IfStmt3 { $1 }


IfStmt3 :: { IfStmt }
IfStmt3 : 'if' BExp 'then' Stmt 'elif' BExp 'then' Stmt 'else' Stmt 'endif' { IfElifElse $2 $4 $6 $8 $10 } 
  | '(' IfStmt ')' { $2 }


Procc :: { Procc }
Procc : Ident '(' ')' ';' { ProcCall $1 } 
  | Ident '(' Ident ')' ';' { ProcCallId $1 $3 }
  | Ident '(' Ident '[' Integer ']' ')' ';' { ProcCallIdArray $1 $3 $5 }
  | Ident '(' Exp ')' ';' { ProcCallExp $1 $3 }
  | Ident '(' BExp ')' ';' { ProcCallBExp $1 $3 }
  | Ident '(' String ')' ';' { ProcCallString $1 $3 }
  | Ident '(' FuncDeclLine ')' ';' { ProcCallFuncSyg $1 $3 }


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
  | Ident '(' ')' { EFunNonArg $1 }
  | Ident '(' Ident ')' { EFunId $1 $3 }
  | Ident '(' Ident '[' Integer ']' ')' { EFunIdArray $1 $3 $5 }
  | Ident '(' Ident ')' { EFunIdWholeArray $1 $3 }
  | Ident '(' Exp ')' { EFunExp $1 $3 }
  | Ident '(' BExp ')' { EFunBExp $1 $3 }
  | Ident '(' String ')' { EFunString $1 $3 }
  | Ident '[' Integer ']' { EArray $1 $3 }
  | '(' Exp ')' { $2 }


BExp :: { BExp }
BExp : BExp '||' BExp1 { BOr $1 $3 } 
  | Exp { BTExp $1 }
  | BExp1 { $1 }


BExp1 :: { BExp }
BExp1 : BExp1 '&&' BExp2 { BAnd $1 $3 } 
  | BExp2 { $1 }


BExp2 :: { BExp }
BExp2 : Exp RelOp Exp { BRel $1 $2 $3 } 
  | Ident '.equals' '(' String ')' { BStringRel $1 $4 }
  | Ident '[' Integer ']' '.equals' '(' String ')' { BStringRel5 $1 $3 $7 }
  | Ident '[' Integer ']' '.equals' '(' Ident ')' { BStringRel6 $1 $3 $7 }
  | String '.equals' '(' Ident ')' { BStringRel2 $1 $4 }
  | Ident '.equals' '(' Ident ')' { BStringRel3 $1 $4 }
  | String '.equals' '(' String ')' { BStringRel4 $1 $4 }
  | BoolLit { BLit $1 }
  | Ident { BIdent $1 }
  | Ident '[' Integer ']' { BExpArray $1 $3 }
  | '(' BExp ')' { $2 }


RelOp :: { RelOp }
RelOp : '<' { LTH } 
  | '<=' { LE }
  | '>' { GTH }
  | '>=' { GE }
  | '==' { EQU }
  | '!=' { NE }


Type :: { Type }
Type : 'Void' { TVoid } 
  | 'Integer' { TInt }
  | 'Boolean' { TBool }
  | 'String' { TString }
  | 'Char' { TChar }
  | 'Double' { TDouble }
  | 'Function' '(' Type ')' ':' Type { TFunc $3 $6 }
  | 'Record' ListVarDeclarationLine 'RecEnd' { TRecord $2 }
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

