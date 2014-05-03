module Absactualgrammar where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Programm Block
  deriving (Eq,Ord,Show)

data Block =
   Blockk VariableDeclaration Block
 | Blockk2 [Stmt]
  deriving (Eq,Ord,Show)

data VariableDeclaration =
   VBExists DeclarationLines
 | VBDoesntExists
  deriving (Eq,Ord,Show)

data DeclarationLines =
   DLList Ident Type
  deriving (Eq,Ord,Show)

data Stmt =
   Stmtt Exp
  deriving (Eq,Ord,Show)

data Exp =
   EAss Ident Exp
  deriving (Eq,Ord,Show)

data Type =
   TInt
 | TBool
  deriving (Eq,Ord,Show)

