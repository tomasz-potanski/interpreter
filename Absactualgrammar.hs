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
   Blockk VariableDeclaration Stmt
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
   SBlock [Stmt]
 | SAss Ident Exp
 | SAssArray Ident Integer Exp
 | SAssBool Ident BExp
 | SAssBoolLit Ident BoolLit
 | SAssString Ident String
 | SAssStrToInt Ident String
 | SAssArrayBool Ident Integer BExp
 | SAssArrayBoolLit Ident Integer BoolLit
 | SAssMult Ident Exp
 | SAssDiv Ident Exp
 | SAssAdd Ident Exp
 | SAssSub Ident Exp
 | SPreIncr Ident
 | SPreDecr Ident
 | SIf IfStmt
 | SWhile BExp Stmt
 | SFor Ident Exp Exp Stmt
 | SPrintExp Exp
 | SPrint LitVal
  deriving (Eq,Ord,Show)

data IfStmt =
   SimpleIf BExp Stmt
 | IfElse BExp Stmt Stmt
 | IfElif BExp Stmt BExp Stmt
 | IfElifElse BExp Stmt BExp Stmt Stmt
  deriving (Eq,Ord,Show)

data Exp =
   EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EInt Integer
 | EId Ident
 | EArray Ident Integer
  deriving (Eq,Ord,Show)

data BExp =
   BOr BExp BExp
 | BAnd BExp BExp
 | BRel Exp RelOp Exp
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
   TInt
 | TBool
 | TString
 | TChar
 | TDouble
 | TArray Integer Integer Type
  deriving (Eq,Ord,Show)

data LitVal =
   LiteralValueString String
 | LiteralValueDouble Double
 | LiteralValueChar Char
  deriving (Eq,Ord,Show)

