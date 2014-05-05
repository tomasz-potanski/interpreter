{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Printactualgrammar where

-- pretty-printer generated by the BNF converter

import Absactualgrammar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   Programm programnameheader block -> prPrec i 0 (concatD [prt 0 programnameheader , prt 0 block , doc (showString ".")])


instance Print ProgramNameHeader where
  prt i e = case e of
   ProgNameHeaderNotBlank id -> prPrec i 0 (concatD [doc (showString "program") , prt 0 id , doc (showString ";")])
   ProgNameHeaderBlank  -> prPrec i 0 (concatD [])


instance Print Block where
  prt i e = case e of
   Blockk variabledeclaration stmt -> prPrec i 0 (concatD [prt 0 variabledeclaration , prt 2 stmt])


instance Print VariableDeclaration where
  prt i e = case e of
   VBExists vardeclarationlines -> prPrec i 0 (concatD [doc (showString "var") , prt 0 vardeclarationlines])
   VBDoesntExists  -> prPrec i 0 (concatD [])


instance Print VarDeclarationLine where
  prt i e = case e of
   DLList ids type' -> prPrec i 0 (concatD [prt 0 ids , doc (showString ":") , prt 0 type' , doc (showString ";")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print ConstantDeclaration where
  prt i e = case e of
   ConstDeclBlank  -> prPrec i 0 (concatD [])
   ConstDeclNotBlank constdecllines -> prPrec i 0 (concatD [doc (showString "const") , prt 0 constdecllines])


instance Print ConstDeclLine where
  prt i e = case e of
   ConsDeclLine id literalvalue -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 literalvalue , doc (showString ";")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print LiteralValue where
  prt i e = case e of
   LiteralValInt n -> prPrec i 0 (concatD [prt 0 n])


instance Print BoolLit where
  prt i e = case e of
   BoolLitTrue  -> prPrec i 0 (concatD [doc (showString "True")])
   BoolLitFalse  -> prPrec i 0 (concatD [doc (showString "False")])


instance Print Stmt where
  prt i e = case e of
   SBlock stmts -> prPrec i 2 (concatD [doc (showString "begin") , prt 0 stmts , doc (showString "end")])
   SAss id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString ":=") , prt 0 exp , doc (showString ";")])
   SAssArray id n exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "[") , prt 0 n , doc (showString "]") , doc (showString ":=") , prt 0 exp , doc (showString ";")])
   SAssBool id bexp -> prPrec i 0 (concatD [prt 0 id , doc (showString ":=") , prt 0 bexp , doc (showString ";")])
   SAssBoolLit id boollit -> prPrec i 0 (concatD [prt 0 id , doc (showString ":=") , prt 0 boollit , doc (showString ";")])
   SAssArrayBool id n bexp -> prPrec i 0 (concatD [prt 0 id , doc (showString "[") , prt 0 n , doc (showString "]") , doc (showString ":=") , prt 0 bexp , doc (showString ";")])
   SAssArrayBoolLit id n boollit -> prPrec i 0 (concatD [prt 0 id , doc (showString "[") , prt 0 n , doc (showString "]") , doc (showString ":=") , prt 0 boollit , doc (showString ";")])
   SAssMult id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "*=") , prt 0 exp , doc (showString ";")])
   SAssDiv id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "/=") , prt 0 exp , doc (showString ";")])
   SAssAdd id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "+=") , prt 0 exp , doc (showString ";")])
   SAssSub id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "-=") , prt 0 exp , doc (showString ";")])
   SPreIncr id -> prPrec i 1 (concatD [doc (showString "++") , prt 0 id , doc (showString ";")])
   SPreDecr id -> prPrec i 1 (concatD [doc (showString "--") , prt 0 id , doc (showString ";")])
   SIf ifstmt -> prPrec i 0 (concatD [prt 0 ifstmt])
   SWhile bexp stmt -> prPrec i 0 (concatD [doc (showString "while") , prt 0 bexp , doc (showString "do") , prt 0 stmt])
   SFor id exp0 exp stmt -> prPrec i 0 (concatD [doc (showString "for") , prt 0 id , doc (showString ":=") , prt 0 exp0 , doc (showString "to") , prt 0 exp , doc (showString "do") , prt 0 stmt])
   SPrintId id -> prPrec i 0 (concatD [doc (showString "print") , prt 0 id , doc (showString ";")])
   SPrint litval -> prPrec i 0 (concatD [doc (showString "print") , prt 0 litval , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print IfStmt where
  prt i e = case e of
   SimpleIf bexp stmt -> prPrec i 0 (concatD [doc (showString "if") , prt 0 bexp , doc (showString "then") , prt 0 stmt])
   IfElse bexp stmt0 stmt -> prPrec i 1 (concatD [doc (showString "if") , prt 0 bexp , doc (showString "then") , prt 0 stmt0 , doc (showString "else") , prt 0 stmt])
   IfElif bexp0 stmt1 bexp stmt -> prPrec i 2 (concatD [doc (showString "if") , prt 0 bexp0 , doc (showString "then") , prt 0 stmt1 , doc (showString "elif") , prt 0 bexp , doc (showString "then") , prt 0 stmt])
   IfElifElse bexp0 stmt1 bexp stmt2 stmt -> prPrec i 3 (concatD [doc (showString "if") , prt 0 bexp0 , doc (showString "then") , prt 0 stmt1 , doc (showString "elif") , prt 0 bexp , doc (showString "then") , prt 0 stmt2 , doc (showString "else") , prt 0 stmt])


instance Print Exp where
  prt i e = case e of
   EAdd exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "+") , prt 1 exp])
   ESub exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "-") , prt 1 exp])
   EMul exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "*") , prt 2 exp])
   EDiv exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "/") , prt 2 exp])
   EInt n -> prPrec i 2 (concatD [prt 0 n])
   EId id -> prPrec i 2 (concatD [prt 0 id])
   EArray id n -> prPrec i 2 (concatD [prt 0 id , doc (showString "[") , prt 0 n , doc (showString "]")])


instance Print BExp where
  prt i e = case e of
   BOr bexp0 bexp -> prPrec i 0 (concatD [prt 0 bexp0 , doc (showString "||") , prt 1 bexp])
   BAnd bexp0 bexp -> prPrec i 1 (concatD [prt 1 bexp0 , doc (showString "&&") , prt 2 bexp])
   BRel exp0 relop exp -> prPrec i 2 (concatD [prt 0 exp0 , prt 0 relop , prt 0 exp])


instance Print RelOp where
  prt i e = case e of
   LTH  -> prPrec i 0 (concatD [doc (showString "<")])
   LE  -> prPrec i 0 (concatD [doc (showString "<=")])
   GTH  -> prPrec i 0 (concatD [doc (showString ">")])
   GE  -> prPrec i 0 (concatD [doc (showString ">=")])
   EQU  -> prPrec i 0 (concatD [doc (showString "==")])
   NE  -> prPrec i 0 (concatD [doc (showString "!=")])


instance Print Type where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "Integer")])
   TBool  -> prPrec i 0 (concatD [doc (showString "Boolean")])
   TString  -> prPrec i 0 (concatD [doc (showString "String")])
   TChar  -> prPrec i 0 (concatD [doc (showString "Char")])
   TDouble  -> prPrec i 0 (concatD [doc (showString "Double")])
   TArray n0 n type' -> prPrec i 2 (concatD [doc (showString "Array") , doc (showString "[") , prt 0 n0 , doc (showString "..") , prt 0 n , doc (showString "]") , doc (showString "of") , prt 0 type'])


instance Print LitVal where
  prt i e = case e of
   LiteralValueInteger n -> prPrec i 0 (concatD [prt 0 n])
   LiteralValueString str -> prPrec i 0 (concatD [prt 0 str])
   LiteralValueDouble d -> prPrec i 0 (concatD [prt 0 d])
   LiteralValueChar c -> prPrec i 0 (concatD [prt 0 c])



