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
  Blockk variabledeclaration procdeclaration stmt  -> failure x


transProcDeclaration :: ProcDeclaration -> Result
transProcDeclaration x = case x of
  PExists procdecllines  -> failure x
  PFExists procdecllines funcdecllines  -> failure x
  FExists funcdecllines  -> failure x
  PDoesntExist  -> failure x


transProcDeclLine :: ProcDeclLine -> Result
transProcDeclLine x = case x of
  PLineNonArg id variabledeclaration stmt  -> failure x
  PLineArg id vardeclarationline variabledeclaration stmt  -> failure x


transFuncDeclLine :: FuncDeclLine -> Result
transFuncDeclLine x = case x of
  FLineNonArg id type' variabledeclaration stmt  -> failure x
  FLineArg id vardeclarationline type' variabledeclaration stmt  -> failure x


transFuncArg :: FuncArg -> Result
transFuncArg x = case x of
  NonEmptyArgs vardeclarationline  -> failure x
  EmptyArgs  -> failure x


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


transBoolLit :: BoolLit -> Result
transBoolLit x = case x of
  BoolLitTrue  -> failure x
  BoolLitFalse  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  SBlock stmts  -> failure x
  SAss id exp  -> failure x
  SAssArray id n exp  -> failure x
  SAssBool id bexp  -> failure x
  SAssBoolLit id boollit  -> failure x
  SReturn n  -> failure x
  SAssString id str  -> failure x
  SAssStrToInt id str  -> failure x
  SAzs id n  -> failure x
  SAssArrayBool id n bexp  -> failure x
  SAssArrayBoolLit id n boollit  -> failure x
  SAssArrayString id n str  -> failure x
  SAssMult id exp  -> failure x
  SAssDiv id exp  -> failure x
  SAssAdd id exp  -> failure x
  SAssSub id exp  -> failure x
  SPreIncr id  -> failure x
  SPreDecr id  -> failure x
  SIf ifstmt  -> failure x
  SWhile bexp stmt  -> failure x
  SFor id exp1 exp2 stmt3  -> failure x
  SPrintString str  -> failure x
  SPrintBLit boollit  -> failure x
  SPrintId id  -> failure x
  SPrintArray id n  -> failure x
  SPrintExp exp  -> failure x
  SPrintBExp bexp  -> failure x
  SPrintCharLit c  -> failure x
  SPrintFun id  -> failure x
  SProcCall id  -> failure x
  SProcCallInteger id n  -> failure x
  SProcCallId id1 id2  -> failure x
  SProcCallIdArray id1 id2 n3  -> failure x
  SProcCallExp id exp  -> failure x
  SProcCallBExp id bexp  -> failure x
  SProcCallString id str  -> failure x


transIfStmt :: IfStmt -> Result
transIfStmt x = case x of
  SimpleIf bexp stmt  -> failure x
  IfElse bexp stmt1 stmt2  -> failure x
  IfElif bexp1 stmt2 bexp3 stmt4  -> failure x
  IfElifElse bexp1 stmt2 bexp3 stmt4 stmt5  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EAdd exp1 exp2  -> failure x
  ESub exp1 exp2  -> failure x
  EMul exp1 exp2  -> failure x
  EDiv exp1 exp2  -> failure x
  EInt n  -> failure x
  EId id  -> failure x
  EFunNonArg id  -> failure x
  EFunInteger id n  -> failure x
  EArray id n  -> failure x


transBExp :: BExp -> Result
transBExp x = case x of
  BOr bexp1 bexp2  -> failure x
  BAnd bexp1 bexp2  -> failure x
  BRel exp1 relop2 exp3  -> failure x
  BLit boollit  -> failure x
  BIdent id  -> failure x
  BExpArray id n  -> failure x
  BTExp exp  -> failure x


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
  TDouble  -> failure x
  TArray n1 n2 type'3  -> failure x


transLitVal :: LitVal -> Result
transLitVal x = case x of
  LiteralValueString str  -> failure x
  LiteralValueDouble d  -> failure x
  LiteralValueChar c  -> failure x



