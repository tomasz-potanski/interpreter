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



instance Print Program where
  prt i e = case e of
   Programm block -> prPrec i 0 (concatD [prt 0 block , doc (showString ".")])


instance Print Block where
  prt i e = case e of
   Blockk variabledeclaration block -> prPrec i 0 (concatD [prt 0 variabledeclaration , prt 2 block])
   Blockk2 stmts -> prPrec i 2 (concatD [doc (showString "begin") , prt 0 stmts , doc (showString "end")])


instance Print VariableDeclaration where
  prt i e = case e of
   VBExists declarationlines -> prPrec i 0 (concatD [doc (showString "var") , prt 0 declarationlines])
   VBDoesntExists  -> prPrec i 0 (concatD [])


instance Print DeclarationLines where
  prt i e = case e of
   DLList id type' -> prPrec i 0 (concatD [prt 0 id , doc (showString ":") , prt 0 type'])


instance Print Stmt where
  prt i e = case e of
   Stmtt exp -> prPrec i 0 (concatD [prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   EAss id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])


instance Print Type where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "integer")])
   TBool  -> prPrec i 0 (concatD [doc (showString "boolean")])



