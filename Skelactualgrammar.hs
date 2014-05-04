module Skelactualgrammar where

-- Haskell module generated by the BNF converter

import Absactualgrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Programm programnameheader block  -> failure x


transProgramNameHeader :: ProgramNameHeader -> Result
transProgramNameHeader x = case x of
  ProgNameHeaderNotBlank id  -> failure x
  ProgNameHeaderBlank  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Blockk constantdeclaration variabledeclaration stmts  -> failure x


transVariableDeclaration :: VariableDeclaration -> Result
transVariableDeclaration x = case x of
  VBExists vardeclarationlines  -> failure x
  VBDoesntExists  -> failure x


transVarDeclarationLine :: VarDeclarationLine -> Result
transVarDeclarationLine x = case x of
  DLList ids type'  -> failure x


transConstantDeclaration :: ConstantDeclaration -> Result
transConstantDeclaration x = case x of
  ConstDeclBlank  -> failure x
  ConstDeclNotBlank constdecllines  -> failure x


transConstDeclLine :: ConstDeclLine -> Result
transConstDeclLine x = case x of
  ConsDeclLine id literalvalue  -> failure x


transLiteralValue :: LiteralValue -> Result
transLiteralValue x = case x of
  LiteralValInt n  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  SBlock stmts  -> failure x
  SAss id exp  -> failure x
  SIf bexp stmt  -> failure x
  SWhile bexp stmt  -> failure x
  SPrint str  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EAdd exp1 exp2  -> failure x
  ESub exp1 exp2  -> failure x
  EMul exp1 exp2  -> failure x
  EDiv exp1 exp2  -> failure x
  EInt n  -> failure x
  EId id  -> failure x


transBExp :: BExp -> Result
transBExp x = case x of
  BOr bexp1 bexp2  -> failure x
  BAnd bexp1 bexp2  -> failure x
  BRel exp1 relop2 exp3  -> failure x


transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH  -> failure x
  LE  -> failure x
  GTH  -> failure x
  GE  -> failure x
  EQU  -> failure x
  NE  -> failure x


transType :: Type -> Result
transType x = case x of
  TInt  -> failure x
  TBool  -> failure x
  TString  -> failure x
  TChar  -> failure x


transLitVal :: LitVal -> Result
transLitVal x = case x of
  LiteralValueInteger n  -> failure x
  LiteralValueString str  -> failure x
  LiteralValueDouble d  -> failure x
  LiteralValueChar c  -> failure x



