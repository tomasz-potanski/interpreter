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
data TTypes = TTInt Integer | TTBoolean Bool | TTArray Integer Integer Type (M.Map Integer TTypes) deriving (Eq, Show)
-- nazwa zmiennej -> wartosc
type TState = M.Map String TTypes
--type ProcMap = M.Map String ([Stmts], [Arg])

extractInt :: TTypes -> Integer
extractInt (TTInt a) = a
extractInt (TTBoolean a) = case a of
	False -> 0
	True -> 1 
extractInt (TTArray _ _ _ _) = 0

extractBool :: TTypes -> Bool
extractBool (TTBoolean a) = a

extractArray :: TTypes -> (Integer, Integer, Type, (M.Map Integer TTypes))
extractArray (TTArray minn maxx typee mapp) = (minn, maxx, typee, mapp)

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
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad
  EArray (Ident x) index -> case (M.lookup x s) of
	Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			case (M.lookup index mapp) of 
			    Just val -> 
				(extractInt val)
			    Nothing -> 0
		    else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	Nothing -> error ("Error - Variable: " ++ (show x) ++ " has not been declared!")
--  EBool b -> case (interpretBExp b s) of 
--		True -> True
--		False -> False

--  EId (Ident x) -> case M.lookup x s of
--	Just n 	-> n
--	Nothing	-> 0 -- !!rzuc blad

variableValueBool :: Ident -> TState -> Bool
variableValueBool (Ident x) s = case M.lookup x s of
	Just n 	-> (extractBool n) 
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad

variableValueInt :: Ident -> TState -> Integer
variableValueInt (Ident x) s = case M.lookup x s of
	Just n 	-> case n of
		TTInt a -> a
		TTBoolean b -> case b of
			False -> 0
			True -> 1
--	Nothing	-> showError ("Zmienna: " ++ (show x) ++ " nie istnieje!") 0 -- !!rzuc blad
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad


----------------BOOLEAN EXPRESSIONS-------------
interpretBExp :: BExp -> TState -> Bool
interpretBExp b s = case b of
	BOr bexp1 bexp2 -> (interpretBExp bexp1 s) || (interpretBExp bexp2 s) 
	BAnd bexp1 bexp2 -> (interpretBExp bexp1 s) && (interpretBExp bexp2 s) 
	BBLit bl -> case bl of
		BoolLitTrue -> True
		BoolLitFalse -> False
	BRel exp1 relOp exp2 -> case relOp of
		LTH -> (interpretExp exp1 s) < (interpretExp exp2 s)
		LE -> (interpretExp exp1 s) <= (interpretExp exp2 s)
		GTH -> (interpretExp exp1 s) > (interpretExp exp2 s)
		GE -> (interpretExp exp1 s) >= (interpretExp exp2 s)
		EQU -> (interpretExp exp1 s) == (interpretExp exp2 s)
		NE -> (interpretExp exp1 s) /= (interpretExp exp2 s)
		
-----------------STATEMETNS----------------
interpretStmts :: [Stmt] -> TState -> TState
interpretStmts [] s = s
interpretStmts (h:tl) s = (interpretStmts tl (interpretStmt h s))

interpretStmt :: Stmt -> TState -> TState
interpretStmt stmt s = case stmt of
    SAss (Ident x) exp -> case (checkifVarExists (Ident x) s) of  
	True ->
        	let val = (interpretExp exp s)
        	in M.insert x (TTInt val) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssArray (Ident x) index exp -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x s) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			M.insert x (TTArray minn maxx typee (M.insert index (TTInt (interpretExp exp s)) mapp)) s
		    else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

--    SAssBoolLit (Ident x) bLit -> case (checkifVarExists (Ident x) s) of  
--	True ->
--		case bLit of
--			BoolLitTrue -> M.insert x (TTBoolean True) s
--			BoolLitFalse -> M.insert x (TTBoolean False) s
--	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssBool (Ident x) bexp -> case (checkifVarExists (Ident x) s) of  
	True ->
		case (interpretBExp bexp s) of 
			True -> M.insert x (TTBoolean True) s
			False -> M.insert x (TTBoolean False) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssArrayBool (Ident x) index bexp -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x s) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			M.insert x (TTArray minn maxx typee (M.insert index (TTBoolean (interpretBExp bexp s)) mapp)) s
		    else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssMult (Ident x) exp -> case (checkifVarExists (Ident x) s) of     
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in M.insert x (TTInt (valL*valR)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssDiv (Ident x) exp -> case (checkifVarExists (Ident x) s) of                -- !! SPRAWDZ DZIELENIE PRZEZ ZERO
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in case valL of
			0 -> error ("Division by zero!")
			otherwise -> M.insert x (TTInt (valL `div` valR)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssAdd (Ident x) exp -> case (checkifVarExists (Ident x) s) of
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in M.insert x (TTInt (valL + valR)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssSub (Ident x) exp -> case (checkifVarExists (Ident x) s) of
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in M.insert x (TTInt (valL - valR)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SPreIncr (Ident x) -> case (checkifVarExists (Ident x) s) of
	True -> let valL = (variableValueInt (Ident x) s)
		in 
			M.insert x (TTInt (valL + 1)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SPreDecr (Ident x) -> case (checkifVarExists (Ident x) s) of
	True -> let valL = (variableValueInt (Ident x) s)
		in 
			M.insert x (TTInt (valL - 1)) s
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

--    SIf b stmt ->
--        let cond = (interpretBExp b s)
--        in if cond then (interpretStmt stmt s) else s
    SIf kind -> case kind of
	SimpleIf b stmt ->
            let cond = (interpretBExp b s)
            in if cond then (interpretStmt stmt s) else s
	IfElse b stmt1 stmt2 ->
            let cond = (interpretBExp b s)
            in if cond then (interpretStmt stmt1 s) else (interpretStmt stmt2 s)
	IfElif b1 stmt1 b2 stmt2 ->
	    let
	        cond1 = (interpretBExp b1 s)
	        cond2 = (interpretBExp b2 s)
	     in
		if cond1 then (interpretStmt stmt1 s) else 
		(if cond2 then (interpretStmt stmt2 s) else s)
	IfElifElse b1 stmt1 b2 stmt2 stmt3 ->
	    let
	        cond1 = (interpretBExp b1 s)
	        cond2 = (interpretBExp b2 s)
	     in
		if cond1 then (interpretStmt stmt1 s) else 
		(if cond2 then (interpretStmt stmt2 s) else (interpretStmt stmt3 s))

    SWhile b i ->
        let cond = (interpretBExp b s)
        in if cond then (interpretStmt stmt (interpretStmt i s)) else s
    SFor (Ident ident) exp0 exp1 stmt ->
	let
	  v1 = interpretExp exp0 s
	  v2 = interpretExp exp1 s
	  new_state = M.insert ident (TTInt v1) s
	  forfor n s = if (n < 0) then s else forfor (n-1) (interpretStmt stmt (M.insert ident (TTInt (v2 - n)) s))
	in
   	  if (v1 < v2) then forfor (v2 - v1) new_state else s    
    SBlock [] -> s
    SBlock (i:is) -> 
        (interpretStmts is (interpretStmt i s))
	-- !! ZROBIĆ PRINT INACZEJ

    SPrint printable -> case printable of
	SPId (Ident x) 	-> case (checkifVarExists (Ident x) s) of
	    False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	    True -> case (M.lookup x s) of
		Just n -> case n of
			TTInt i 	-> showToUser (show i) s
			TTBoolean b 	-> 
				if b then showToUser "True" s
				else showToUser "False" s
		Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
--	SPExp exp 	-> showToUser (show (interpretExp exp s)) s

--    SPrintExp exp -> 
--	
--    SPrintId (Ident x) -> case (checkifVarExists (Ident x) s) of
--	True -> --showToUser (show (variableValueInt (Ident x) s)) s
--	    case (M.lookup x s) of
--		TTInt i 	-> showToUser (show i) s
--		TTBoolean b 	-> 
--			if b then showToUser "True" s
--			else showToUser "False" s
--	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
 --   SPrint a -> case a of
--		LiteralValueString ss -> showToUser ss s
--		LiteralValueInteger ii -> showToUser (show ii) s  
--		LiteralValueChar ss -> showToUser [ss] s
--		LiteralValueDouble ii -> showToUser (show ii) s 
--
checkifVarExists :: Ident -> TState -> Bool
checkifVarExists (Ident ident) state = case M.lookup ident state of
	Just n 	-> True
	Nothing	-> False

checkifVarExistsAndIsArray :: Ident -> TState -> Bool
checkifVarExistsAndIsArray (Ident ident) state = case M.lookup ident state of
	Just n 	-> case n of
			TTArray _ _ _ _	-> True
			otherwise 	-> False
	Nothing	-> False

addOneVariable :: Ident -> Type -> TState -> TState
addOneVariable (Ident ident) typee state = case typee of
		TInt -> M.insert ident (TTInt 0) state
		TBool -> M.insert ident (TTBoolean False) state
		TArray minn maxx typee -> 
			if (minn < maxx) && (minn >= 0) then 
				M.insert ident (TTArray minn maxx typee M.empty) state
			else 
				error("Error - incorrect table index range!")	
	
addManyVariables :: [Ident] -> Type -> TState -> TState
addManyVariables [] typee state = state 
addManyVariables ((Ident ident):tl) typee state = 
	addManyVariables tl typee (addOneVariable (Ident ident) typee state)

declareNewVariables :: VariableDeclaration -> TState -> TState
declareNewVariables vars state = case vars of
	VBDoesntExists -> state
	VBExists [] -> state
	VBExists listOfVarDecl@((DLList idents typee):tl) -> 
--		let s = addOneVariable (Ident ident) typee state
		let s = addManyVariables idents typee state
		in
			declareNewVariables (VBExists tl) s

-------------INTERPRET FILE------------
interpretFile :: Program -> TState
interpretFile (Programm programNameHeader (Blockk variableDeclaration stmts)) = interpretStmt stmts (declareNewVariables variableDeclaration M.empty)
--interpretFile :: Program -> TState