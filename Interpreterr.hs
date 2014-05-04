module Interpreterr where
import Absactualgrammar
import qualified Data.Map as M
--------------------------------
--------------------------------
--------------------------------
--------Tomasz Potański---------
----------321 150---------------
-----interpreter, JiPP----------
--------------------------------
--------------------------------
--------------------------------

import System.IO
import System.IO.Unsafe
import Debug.Trace


----types-------------
--data TTypes = TTInt Integer | TTBoolean Bool | TTString String | TTChar Char | TTDouble Double deriving (Eq, Show)
data TTypes = TTInt Integer | TTBoolean Bool deriving (Eq, Show)
-- nazwa zmiennej -> wartosc
type TState = M.Map String TTypes
--type ProcMap = M.Map String ([Stmts], [Arg])

extractInt :: TTypes -> Integer
extractInt (TTInt a) = a

extractBool :: TTypes -> Bool
extractBool (TTBoolean a) = a

--extractString :: TTypes -> String
--extractString (TTString a) = a

--extractChar :: TTypes -> Char
--extractChar (TTChar a) = a

--extractDouble :: TTypes -> Double
--extractDouble (TTDouble a) = a


--import Control.Monad.State
--import Control.Monad.Error

-------------additional io--------
putIO :: String -> IO ()
putIO msg = do
	putStrLn msg

showToUser :: String -> a -> a
showToUser string expr = unsafePerformIO $ do
	putIO string
--	putStrLn string
	return expr



-----------------EXPRESSIONS--------------------
interpretExp :: Exp -> TState -> Integer
interpretExp x s = case x of
  EAdd exp0 exp  -> (interpretExp exp0 s) + (interpretExp exp s)
  ESub exp0 exp  -> (interpretExp exp0 s) - (interpretExp exp s)
  EMul exp0 exp  -> (interpretExp exp0 s) * (interpretExp exp s)
  EDiv exp0 exp  -> let r = (interpretExp exp s) in
			case r of
				0 -> error("Division by zero!")
				otherwise -> (interpretExp exp0 s) `div` r -- !! SPRAWDZ DZIELENIE PRZEZ ZERO
  EInt n  -> n
  EId (Ident x) -> case M.lookup x s of
	Just n 	-> case n of
		TTInt a -> a
		TTBoolean b -> case b of
			False -> 0
			True -> 1
	Nothing	-> error ("Zmienna: " ++ (show x) ++ " nie istnieje!") -- !!rzuc blad

--  EId (Ident x) -> case M.lookup x s of
--	Just n 	-> n
--	Nothing	-> 0 -- !!rzuc blad

variableValueBool :: Ident -> TState -> Bool
variableValueBool (Ident x) s = case M.lookup x s of
	Just n 	-> (extractBool n) 
	Nothing	-> error ("Zmienna: " ++ (show x) ++ " nie istnieje!") -- !!rzuc blad

variableValueInt :: Ident -> TState -> Integer
variableValueInt (Ident x) s = case M.lookup x s of
	Just n 	-> case n of
		TTInt a -> a
		TTBoolean b -> case b of
			False -> 0
			True -> 1
--	Nothing	-> showError ("Zmienna: " ++ (show x) ++ " nie istnieje!") 0 -- !!rzuc blad
	Nothing	-> error ("Zmienna: " ++ (show x) ++ " nie istnieje!") -- !!rzuc blad


----------------BOOLEAN EXPRESSIONS-------------
interpretBExp :: BExp -> TState -> Bool
interpretBExp b s = case b of
	BOr bexp1 bexp2 -> (interpretBExp bexp1 s) || (interpretBExp bexp2 s) 
	BAnd bexp1 bexp2 -> (interpretBExp bexp1 s) && (interpretBExp bexp2 s) 
	BRel exp1 relOp exp2 -> case relOp of
		LTH -> (interpretExp exp1 s) < (interpretExp exp2 s)
		LE -> (interpretExp exp1 s) <= (interpretExp exp2 s)
		GTH -> (interpretExp exp1 s) > (interpretExp exp2 s)
		GE -> (interpretExp exp1 s) >= (interpretExp exp2 s)
		EQU -> (interpretExp exp1 s) == (interpretExp exp2 s)
		NE -> (interpretExp exp1 s) /= (interpretExp exp2 s)
		
-----------------STATEMETNS----------------
interpretStmt :: Stmt -> TState -> TState
interpretStmt stmt s = case stmt of
    SAss (Ident x) exp ->
        let val = (interpretExp exp s)
        in M.insert x (TTInt val) s
    SAssBoolLit (Ident x) bLit ->
	case bLit of
		BoolLitTrue -> M.insert x (TTBoolean True) s
		BoolLitFalse -> M.insert x (TTBoolean False) s
    SAssBool (Ident x) bexp ->
	case (interpretBExp bexp s) of 
		True -> M.insert x (TTBoolean True) s
		False -> M.insert x (TTBoolean False) s
    SAssMult (Ident x) exp ->
	let valR = (interpretExp exp s)
	in let valL = (variableValueInt (Ident x) s)
	in M.insert x (TTInt (valL*valR)) s
    SAssDiv (Ident x) exp ->                 -- !! SPRAWDZ DZIELENIE PRZEZ ZERO
	let valR = (interpretExp exp s)
	in let valL = (variableValueInt (Ident x) s)
	in case valL of
		0 -> error ("Division by zero!")
		otherwise -> M.insert x (TTInt (valL `div` valR)) s
    SAssAdd (Ident x) exp ->
	let valR = (interpretExp exp s)
	in let valL = (variableValueInt (Ident x) s)
	in M.insert x (TTInt (valL + valR)) s
    SAssSub (Ident x) exp ->
	let valR = (interpretExp exp s)
	in let valL = (variableValueInt (Ident x) s)
	in M.insert x (TTInt (valL - valR)) s

    SPreIncr (Ident x) ->
	let valL = (variableValueInt (Ident x) s)
	in M.insert x (TTInt (valL + 1)) s
    SPreDecr (Ident x) ->
	let valL = (variableValueInt (Ident x) s)
	in M.insert x (TTInt (valL - 1)) s

    SIf b i ->
        let cond = (interpretBExp b s)
        in if cond then (interpretStmt i s) else s
    SWhile b i ->
        let cond = (interpretBExp b s)
        in if cond then (interpretStmt stmt (interpretStmt i s)) else s
    SBlock [] -> s
    SBlock (i:is) ->
        (interpretStmt (SBlock is) (interpretStmt i s))
	-- !! ZROBIĆ PRINT INACZEJ
    SPrintId (Ident x) -> showToUser (show (variableValueInt (Ident x) s)) s
    SPrint a -> case a of
		LiteralValueString ss -> showToUser ss s
		LiteralValueInteger ii -> showToUser (show ii) s  
		LiteralValueChar ss -> showToUser [ss] s
		LiteralValueDouble ii -> showToUser (show ii) s 

--M.insert x (TTBoolean True) s

addOneVariable :: String -> Type -> TState -> TState
addOneVariable ident type state = case type of
		TInt -> M.insert ident (TTInt 0) state
		TBool -> M.insert ident (TTBoolean False) state

declareNewVariables :: VariableDeclaration -> TState -> TState
declareNewVariables vars state = case vars of
	VBDoesnotExists -> state
	VBExists [] -> state
	VBExists listOfVarDecl@((DLList ident type):tl) -> 
		let s = addOneVariable ident type state
		in
			declareNewVariables (VBExists tl) s

-------------INTERPRET FILE------------
interpretFile :: Program -> TState
interpretFile (Programm programNameHeader (Blockk variableDeclaration stmts)) = interpretStmt stmts (declareNewVariables variableDeclaration M.empty)
--interpretFile :: Program -> TState