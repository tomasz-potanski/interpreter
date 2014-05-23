module Absactualgrammar where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Programm ProgramNameHeader Block
  deriving (Eq,Ord,Show)

data ProgramNameHeader =
   ProgNameHeaderNotBlank Ident
 | ProgNameHeaderBlank
  deriving (Eq,Ord,Show)

data Block =
   Blockk VariableDeclaration ProcDeclaration Stmt
  deriving (Eq,Ord,Show)

data ProcDeclaration =
   PExists [ProcDeclLine]
 | PFExists [ProcDeclLine] [FuncDeclLine]
 | FExists [FuncDeclLine]
 | PDoesntExist
  deriving (Eq,Ord,Show)

data ProcDeclLine =
   PLineNonArg Ident VariableDeclaration Stmt
 | PLineArg Ident VarDeclarationLine VariableDeclaration Stmt
  deriving (Eq,Ord,Show)

data FuncDeclLine =
   FLineNonArg Ident Type VariableDeclaration Stmt
 | FLineArg Ident VarDeclarationLine Type VariableDeclaration Stmt
  deriving (Eq,Ord,Show)

data FuncArg =
   NonEmptyArgs VarDeclarationLine
 | EmptyArgs
  deriving (Eq,Ord,Show)

data VariableDeclaration =
   VBExists [VarDeclarationLine]
 | VBDoesntExists
  deriving (Eq,Ord,Show)

data VarDeclarationLine =
   DLList [Ident] Type
  deriving (Eq,Ord,Show)

data ConstantDeclaration =
   ConstDeclBlank
 | ConstDeclNotBlank [ConstDeclLine]
  deriving (Eq,Ord,Show)

data ConstDeclLine =
   ConsDeclLine Ident LiteralValue
  deriving (Eq,Ord,Show)

data LiteralValue =
   LiteralValInt Integer
  deriving (Eq,Ord,Show)

data BoolLit =
   BoolLitTrue
 | BoolLitFalse
  deriving (Eq,Ord,Show)

data Stmt =
   SBlank
 | SBlock [Stmt]
 | SAttr Ident Ident
 | SAttrArray Ident Integer Ident
 | SAss Ident Exp
 | SAssArray Ident Integer Exp
 | SAssBool Ident BExp
 | SAssBoolLit Ident BoolLit
 | SReturn Integer
 | SAssString Ident String
 | SAssStrToInt Ident String
 | SAzs Ident Integer
 | SAssArrayBool Ident Integer BExp
 | SAssArrayBoolLit Ident Integer BoolLit
 | SAssArrayString Ident Integer String
 | SAssMult Ident Exp
 | SAssDiv Ident Exp
 | SAssAdd Ident Exp
 | SAssSub Ident Exp
 | SPreIncr Ident
 | SPreDecr Ident
 | SIf IfStmt
 | SWhile BExp Stmt
 | SFor Ident Exp Exp Stmt
 | SPrintString String
 | SPrintBLit BoolLit
 | SPrintId Ident
 | SPrintArray Ident Integer
 | SPrintExp Exp
 | SPrintBExp BExp
 | SPrintCharLit Char
 | SPrintFun Ident
 | SPrintFunExp Ident Exp
 | SPrintFunBExp Ident BExp
 | SPrintFunString Ident String
 | SPrintFunId Ident Ident
 | SPrintFunIdArray Ident Ident Integer
 | SProcAttr Ident Procc
 | SProcCall Ident
 | SProcCallId Ident Ident
 | SProcCallIdArray Ident Ident Integer
 | SProcCallExp Ident Exp
 | SProcCallBExp Ident BExp
 | SProcCallString Ident String
 | SProcCallFuncSyg Ident FuncDeclLine
  deriving (Eq,Ord,Show)

data IfStmt =
   SimpleIf BExp Stmt
 | IfElse BExp Stmt Stmt
 | IfElif BExp Stmt BExp Stmt
 | IfElifElse BExp Stmt BExp Stmt Stmt
  deriving (Eq,Ord,Show)

data Procc =
   ProcCall Ident
 | ProcCallId Ident Ident
 | ProcCallIdArray Ident Ident Integer
 | ProcCallExp Ident Exp
 | ProcCallBExp Ident BExp
 | ProcCallString Ident String
 | ProcCallFuncSyg Ident FuncDeclLine
  deriving (Eq,Ord,Show)

data Exp =
   EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EInt Integer
 | EId Ident
 | EFunNonArg Ident
 | EFunId Ident Ident
 | EFunIdArray Ident Ident Integer
 | EFunIdWholeArray Ident Ident
 | EFunExp Ident Exp
 | EFunBExp Ident BExp
 | EFunString Ident String
 | EArray Ident Integer
  deriving (Eq,Ord,Show)

data BExp =
   BOr BExp BExp
 | BAnd BExp BExp
 | BRel Exp RelOp Exp
 | BStringRel Ident String
 | BStringRel5 Ident Integer String
 | BStringRel6 Ident Integer Ident
 | BStringRel2 String Ident
 | BStringRel3 Ident Ident
 | BStringRel4 String String
 | BLit BoolLit
 | BIdent Ident
 | BExpArray Ident Integer
 | BTExp Exp
  deriving (Eq,Ord,Show)

data RelOp =
   LTH
 | LE
 | GTH
 | GE
 | EQU
 | NE
  deriving (Eq,Ord,Show)

data Type =
   TVoid
 | TInt
 | TBool
 | TString
 | TChar
 | TDouble
 | TArray Integer Integer Type
 | TFunc Type Type
  deriving (Eq,Ord,Show)

data LitVal =
   LiteralValueString String
 | LiteralValueDouble Double
 | LiteralValueChar Char
  deriving (Eq,Ord,Show)

