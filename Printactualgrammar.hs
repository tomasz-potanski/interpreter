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
   Blockk constantdeclaration variabledeclaration stmts -> prPrec i 0 (concatD [prt 0 constantdeclaration , prt 0 variabledeclaration , doc (showString "begin") , prt 0 stmts , doc (showString "end")])


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


instance Print Stmt where
  prt i e = case e of
   Stmtt exp -> prPrec i 0 (concatD [prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   EAdd exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "+") , prt 1 exp])
   ESub exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "-") , prt 1 exp])
   EMul exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "*") , prt 2 exp])
   EDiv exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "/") , prt 2 exp])
   EInt n -> prPrec i 2 (concatD [prt 0 n])


instance Print Type where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "Integer")])
   TBool  -> prPrec i 0 (concatD [doc (showString "Boolean")])



