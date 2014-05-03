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
  Programm block  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Blockk variabledeclaration block  -> failure x
  Blockk2 stmts  -> failure x


transVariableDeclaration :: VariableDeclaration -> Result
transVariableDeclaration x = case x of
  VBExists declarationlines  -> failure x
  VBDoesntExists  -> failure x


transDeclarationLines :: DeclarationLines -> Result
transDeclarationLines x = case x of
  DLList ids type'  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  Stmtt exp  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EAss id exp  -> failure x


transType :: Type -> Result
transType x = case x of
  TInt  -> failure x
  TBool  -> failure x



