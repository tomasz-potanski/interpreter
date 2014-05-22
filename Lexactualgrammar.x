-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
module Lexactualgrammar where



import qualified Data.Bits
import Data.Word (Word8)
}


$l = [a-zA-Z\192 - \255] # [\215 \247]    -- isolatin1 letter FIXME
$c = [A-Z\192-\221] # [\215]    -- capital isolatin1 letter FIXME
$s = [a-z\222-\255] # [\247]    -- small isolatin1 letter FIXME
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [\0-\255]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \. | \; | \( | \) | \: | \, | \= | \: \= | \[ | \] | \* \= | \/ \= | \+ \= | \- \= | \+ \+ | \- \- | \< \- | \< \- \- | \+ | \- | \* | \/ | \| \| | \& \& | \. "equals" | \< | \< \= | \> | \> \= | \= \= | \! \= | \. \.

:-
"//" [.]* ; -- Toss single line comments
"#" [.]* ; -- Toss single line comments
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ; 
"{*" ([$u # \*] | \* [$u # \}])* ("*")+ "}" ; 

$white+ ;
@rsyms { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }

$l $i*   { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }
\" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t)))* \"{ tok (\p s -> PT p (TL $ share $ unescapeInitTail s)) }
\' ($u # [\' \\] | \\ [\\ \' n t]) \'  { tok (\p s -> PT p (TC $ share s))  }
$d+      { tok (\p s -> PT p (TI $ share s))    }
$d+ \. $d+ (e (\-)? $d+)? { tok (\p s -> PT p (TD $ share s)) }

{

tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals

 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn (PT p _) = p
tokenPosn (Err p) = p
tokenLineCol = posLineCol . tokenPosn
posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s


data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords = b "Double" 33 (b "/" 17 (b "+=" 9 (b "*" 5 (b "(" 3 (b "&&" 2 (b "!=" 1 N N) N) (b ")" 4 N N)) (b "+" 7 (b "*=" 6 N N) (b "++" 8 N N))) (b "-=" 13 (b "-" 11 (b "," 10 N N) (b "--" 12 N N)) (b ".." 15 (b "." 14 N N) (b ".equals" 16 N N)))) (b "<=" 25 (b ";" 21 (b ":" 19 (b "/=" 18 N N) (b ":=" 20 N N)) (b "<-" 23 (b "<" 22 N N) (b "<--" 24 N N))) (b ">=" 29 (b "==" 27 (b "=" 26 N N) (b ">" 28 N N)) (b "Boolean" 31 (b "Array" 30 N N) (b "Char" 32 N N))))) (b "for" 49 (b "]" 41 (b "String" 37 (b "Function" 35 (b "False" 34 N N) (b "Integer" 36 N N)) (b "Void" 39 (b "True" 38 N N) (b "[" 40 N N))) (b "elif" 45 (b "const" 43 (b "begin" 42 N N) (b "do" 44 N N)) (b "end" 47 (b "else" 46 N N) (b "endif" 48 N N)))) (b "program" 57 (b "of" 53 (b "if" 51 (b "function" 50 N N) (b "intToStr" 52 N N)) (b "proc" 55 (b "print" 54 N N) (b "procedure" 56 N N))) (b "to" 61 (b "strToInt" 59 (b "return" 58 N N) (b "then" 60 N N)) (b "while" 63 (b "var" 62 N N) (b "||" 64 N N)))))
   where b s n = let bs = id s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

  -- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
