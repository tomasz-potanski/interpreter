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
   Blockk ConstantDeclaration VariableDeclaration [Stmt]
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

data Stmt =
   SBlock [Stmt]
 | SAss Ident Exp
 | SAssMult Ident Exp
 | SIf BExp Stmt
 | SWhile BExp Stmt
 | SPrintId Ident
 | SPrint LitVal
  deriving (Eq,Ord,Show)

data Exp =
   EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EInt Integer
 | EId Ident
  deriving (Eq,Ord,Show)

data BExp =
   BOr BExp BExp
 | BAnd BExp BExp
 | BRel Exp RelOp Exp
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
  deriving (Eq,Ord,Show)

data LitVal =
   LiteralValueInteger Integer
 | LiteralValueString String
 | LiteralValueDouble Double
 | LiteralValueChar Char
  deriving (Eq,Ord,Show)

