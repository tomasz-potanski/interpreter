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

data TTypes = TTInt Integer | TTBoolean Bool | TTVoid | TTString String | TTArray Integer Integer Type (M.Map Integer TTypes) deriving (Eq, Show)
-- nazwa zmiennej -> wartosc
type TStateOld = M.Map String TTypes
type TState2 = (TLoc, TEnv, TFuncMap)
type TState3 = (TStateOld, TFuncMap)

-- statements, arguments, return type, Env, Decl)
type TFuncDef2 = (Stmt, VarDeclarationLine, TTypes, TEnv, TStore)
type TFuncDef = (Stmt, FuncArg, TTypes, TStateOld)
type TFuncMap = M.Map String TFuncDef
type TLoc = Integer
type TEnv = M.Map String TLoc
type TStore = M.Map TLoc TTypes



---------helpful funcitons--------


--idAndTypeToVarDecl :: Ident -> Type ->
--idAndTypeToVarDecl (Ident x) typee

typeToDefaultTType :: Type -> TTypes
typeToDefaultTType typee = case typee of
    TInt -> TTInt 0
    TBool -> TTBoolean False
    TString -> TTString ""
    TArray minn maxx ofType -> TTArray minn maxx ofType M.empty


identToString :: Ident -> TState3 -> String
identToString (Ident ident) s@(stateOld, funcMap) =
    case (M.lookup ident stateOld) of
        Nothing -> error("Error - variable has not been found!")
        Just n -> case n of
            TTString s  -> s
            TTInt i     -> (show i)
            TTBoolean b -> if b == True then "True" else "False"
            TTArray minn maxx ofType values -> case (M.lookup minn values) of
                Nothing -> error("Error - value has not been found!")
                Just nn -> case nn of
                    TTString ss  -> ss
                    TTInt ii     -> (show ii)
                    TTBoolean bb -> if bb == True then "True" else "False"

typeCheck :: TTypes -> Type -> Bool
typeCheck ttype typee = case ttype of
    TTBoolean _     ->  if typee == TBool then True else False
    TTInt _         -> if typee == TInt then True else False
    TTString _      -> if typee == TString then True else False
    TTArray _ _ ofType _ -> case typee of
        TArray _ _ ofType2 -> if ofType == ofType2 then True else False
        otherwise -> False

extractString :: TTypes -> String
extractString (TTString s) = s

extractInt :: TTypes -> Integer
extractInt (TTInt a) = a
extractInt (TTBoolean a) = case a of
	False -> 0
	True -> 1 
extractInt (TTArray _ _ _ _) = 0

extractBool :: TTypes -> Bool
extractBool (TTBoolean a) = a

extractBoolLit :: BoolLit -> Bool
extractBoolLit b = case b of
	BoolLitTrue -> True
	BoolLitFalse -> False

boolToInt :: Bool -> Integer
boolToInt b = case b of
	True -> 1
	False -> 0

intToBool :: Integer -> Bool
intToBool i = case i of 
	0 -> False
	otherwise -> True

intToStr :: Integer -> String
intToStr i = (show i)

strToInt :: String -> Integer
strToInt s = read s :: Integer

variableValueBool :: Ident -> TState3 -> Bool
variableValueBool (Ident x) (s, funcMap) = case M.lookup x s of
	Just n 	-> (extractBool n) 
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad

variableValueInt :: Ident -> TState3 -> Integer
variableValueInt (Ident x) (s, funcMap) = case M.lookup x s of
	Just n 	-> case n of
		TTInt a -> a
		TTBoolean b -> case b of
			False -> 0
			True -> 1
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad
     

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

checkifVarExists :: Ident -> TState3 -> Bool
checkifVarExists (Ident ident) (state, funcMap) = case M.lookup ident state of
	Just n 	-> True
	Nothing	-> False

checkifVarExistsAndIsArray :: Ident -> TState3 -> Bool
checkifVarExistsAndIsArray (Ident ident) (state, funcMap) = case M.lookup ident state of
	Just n 	-> case n of
			TTArray _ _ _ _	-> True
			otherwise 	-> False
	Nothing	-> False

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
interpretExp :: Exp -> TState3 -> Integer
interpretExp x s@(state, funcMap) = case x of
  EAdd exp0 exp  -> (interpretExp exp0 s) + (interpretExp exp s)
  ESub exp0 exp  -> (interpretExp exp0 s) - (interpretExp exp s)
  EMul exp0 exp  -> (interpretExp exp0 s) * (interpretExp exp s)
  EDiv exp0 exp  -> let r = (interpretExp exp s) in
			case r of
				0 -> error("Division by zero!")
				otherwise -> (interpretExp exp0 s) `div` r -- !! SPRAWDZ DZIELENIE PRZEZ ZERO
  EInt n  -> n
  EId (Ident x) -> case M.lookup x state of
	Just n 	-> case n of
		TTInt a -> a
		TTBoolean b -> case b of
			False -> 0
			True -> 1
		TTString _ -> 0
		TTArray _ _ _ _ -> 0
	Nothing	-> error ("Error - Variable: " ++ (show x) ++ " has not been declared!") -- !!rzuc blad
  EArray (Ident x) index -> case (M.lookup x state) of
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


----------------BOOLEAN EXPRESSIONS-------------
interpretBExp :: BExp -> TState3 -> Bool
interpretBExp b s@(state, funcMap) = case b of
	BOr bexp1 bexp2 -> (interpretBExp bexp1 s) || (interpretBExp bexp2 s) 
	BAnd bexp1 bexp2 -> (interpretBExp bexp1 s) && (interpretBExp bexp2 s) 
	BTExp exp -> error("Error - specjalnie nie obsuguje rzutowania")
	BLit boolLit -> case boolLit of 
		BoolLitTrue -> True
		BoolLitFalse -> False
	BIdent (Ident x) -> case (checkifVarExists (Ident x) s) of
		True -> case (M.lookup x state) of 
		    Just n -> case n of
			TTBoolean b -> b
			TTInt i -> if i == 0 then False else True
			otherwise -> False
		    Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
		False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

	BExpArray (Ident x) index -> case (checkifVarExistsAndIsArray (Ident x) s) of
		True -> case (M.lookup x state) of 
		    Just n -> case n of
			TTArray minn maxx typee mapp -> 
				if (index >= minn) && (index <= maxx) then
					case (M.lookup index mapp) of
					    Nothing -> False
					    Just m -> case m of
						TTBoolean w -> w
						TTInt w -> if w == 0 then False else True
						otherwise -> False
				else error("Error - array index out of bound!")
			otherwise -> False
		    Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
		False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	
	BRel exp1 relOp exp2 -> case relOp of
		LTH -> (interpretExp exp1 s) < (interpretExp exp2 s)
		LE -> (interpretExp exp1 s) <= (interpretExp exp2 s)
		GTH -> (interpretExp exp1 s) > (interpretExp exp2 s)
		GE -> (interpretExp exp1 s) >= (interpretExp exp2 s)
		EQU -> (interpretExp exp1 s) == (interpretExp exp2 s)
		NE -> (interpretExp exp1 s) /= (interpretExp exp2 s)
		
-----------------STATEMETNS----------------
interpretStmts :: [Stmt] -> TState3 -> TState3
interpretStmts [] s = s
interpretStmts (h:tl) s = (interpretStmts tl (interpretStmt h s))

interpretStmt :: Stmt -> TState3 -> TState3
interpretStmt stmt s@(extState, funcMap) = case stmt of
    SAss (Ident x) exp -> case (checkifVarExists (Ident x) s) of  
	True ->
        	let val = (interpretExp exp s)
        	in (M.insert x (TTInt val) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssStrToInt (Ident x) str -> case (checkifVarExists (Ident x) s) of  
	True -> let val = (strToInt str)
        	in 
		    case (M.lookup x extState) of
			Just n -> case n of
				TTInt _ -> ((M.insert x (TTInt val) extState), funcMap)
				TTString _ -> error("Error - incorrect types")
				TTBoolean _ -> if (val == 0) || (val == 1) then
					((M.insert x (TTBoolean (intToBool val)) extState), funcMap) else s
				otherwise -> error("Error - incorrect types")
			Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAzs (Ident x) intt -> case (checkifVarExists (Ident x) s) of  
	True -> let val = (intToStr intt)
        	in 
		    case (M.lookup x extState) of
			Just n -> case n of
				TTInt _ ->  error("Error - incorrect types")
				TTString _ -> ((M.insert x (TTString val) extState), funcMap)
				TTBoolean _ -> error("Error - incorrect types")
				otherwise -> error("Error - incorrect types")
			Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssString (Ident x) str -> case (checkifVarExists (Ident x) s) of  
	True -> case (M.lookup x extState) of
	    Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	    Just m -> case m of
	        TTString _ -> ((M.insert x (TTString str) extState), funcMap)
	        otherwise -> error("Error - incorrect type - nie rzutujemy")
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssArray (Ident x) index exp -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x extState) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			let val = (interpretExp exp s)
			in
			  case typee of
			     TInt -> (M.insert x (TTArray minn maxx typee (M.insert index (TTInt val) mapp)) extState, funcMap)
			     TBool -> if (val == 0) || (val == 1) then (M.insert x (TTArray minn maxx typee (M.insert index (TTInt val) mapp)) extState, funcMap)
					else error("Error - incorrect type")
			     otherwise -> error("Error - incorrect type")		  
			  else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssBoolLit (Ident x) bLit -> case (checkifVarExists (Ident x) s) of  
	True -> case bLit of
			BoolLitTrue -> (M.insert x (TTBoolean True) extState, funcMap)
			BoolLitFalse -> (M.insert x (TTBoolean False) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssBool (Ident x) bexp -> case (checkifVarExists (Ident x) s) of  
	True ->
		case (interpretBExp bexp s) of 
			True -> (M.insert x (TTBoolean True) extState, funcMap)
			False -> (M.insert x (TTBoolean False) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssArrayBool (Ident x) index bexp -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x extState) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			(M.insert x (TTArray minn maxx typee (M.insert index (TTBoolean (interpretBExp bexp s)) mapp)) extState, funcMap)
		    else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssArrayBoolLit (Ident x) index boolLit -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x extState) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
			let boolVal = extractBoolLit boolLit
			in
			case typee of
		    	   TBool -> (M.insert x (TTArray minn maxx typee (M.insert index (TTBoolean (extractBoolLit boolLit)) mapp)) extState, funcMap)
		    	   TInt -> error("Error - mapa przechowuje Inty - nie chcemy rzutowac")
		    	   otherwise -> error("Error - niepoprawny typ")
		    else 
			error("Error - index out of bound!")
		otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssArrayString (Ident x) index str -> case (checkifVarExistsAndIsArray (Ident x) s) of  
	True 	-> case (M.lookup x extState) of 
	    Just n -> case n of
		TTArray minn maxx typee mapp -> 
		    if (index >= minn) && (index <= maxx) then
                        if typee == TString then (M.insert x (TTArray minn maxx typee (M.insert index (TTString str) mapp)) extState, funcMap)
			else error("Error - incorrect assignment type")
		    else 
			error("Error - index out of bound!")
	        otherwise -> error("Error - variable is not an array!")
	False 	-> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SAssMult (Ident x) exp -> case (checkifVarExists (Ident x) s) of     
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in (M.insert x (TTInt (valL*valR)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssDiv (Ident x) exp -> case (checkifVarExists (Ident x) s) of                -- !! SPRAWDZ DZIELENIE PRZEZ ZERO
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in case valL of
			0 -> error ("Division by zero!")
			otherwise -> (M.insert x (TTInt (valL `div` valR)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssAdd (Ident x) exp -> case (checkifVarExists (Ident x) s) of
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in (M.insert x (TTInt (valL + valR)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SAssSub (Ident x) exp -> case (checkifVarExists (Ident x) s) of
	True ->
		let valR = (interpretExp exp s)
		in let valL = (variableValueInt (Ident x) s)
		in (M.insert x (TTInt (valL - valR)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SPreIncr (Ident x) -> case (checkifVarExists (Ident x) s) of
	True -> let valL = (variableValueInt (Ident x) s)
		in 
			(M.insert x (TTInt (valL + 1)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
    SPreDecr (Ident x) -> case (checkifVarExists (Ident x) s) of
	True -> let valL = (variableValueInt (Ident x) s)
		in 
			(M.insert x (TTInt (valL - 1)) extState, funcMap)
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

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
	  new_state = (M.insert ident (TTInt v1) extState, funcMap)
	  forfor n s = if (n < 0) then s else forfor (n-1) (interpretStmt stmt ((M.insert ident (TTInt (v2 - n)) extState), funcMap))
	in
   	  if (v1 < v2) then forfor (v2 - v1) new_state else s    
    SBlock [] -> s
    SBlock (i:is) -> 
        (interpretStmts is (interpretStmt i s))
	-- !! ZROBIĆ PRINT INACZEJ
    SPrintString str -> showToUser str s
    SPrintBLit bLit -> case bLit of
	BoolLitTrue -> showToUser "True" s
	BoolLitFalse -> showToUser "False" s


    SPrintId (Ident x) -> case (checkifVarExists (Ident x) s) of
	True -> case (M.lookup x extState) of
	    Just mm -> case mm of
	        TTInt val -> (showToUser (intToStr val) s)
		TTBoolean val -> case val of
                    True -> (showToUser "True" s)
                    False -> (showToUser "False" s)
		TTString val -> (showToUser val s)
		otherwise -> error("Error - Variable: " ++ (show x) ++ " is unprintable!")
	    Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
	False -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")

    SPrintArray (Ident x) index -> case (M.lookup x extState) of
        Nothing -> error("Error - Variable: " ++ (show x) ++ " has not been declared!")
        Just n -> case n of
            TTArray minn maxx typee mapp -> 
                if (index >= minn) && (index <= maxx) then
                    case typee of 
                        TInt  -> case (M.lookup index mapp) of
                            Nothing -> showToUser "0" s
                            Just (TTInt nn)-> showToUser (show nn) s
                        TBool  -> case (M.lookup index mapp) of
                            Nothing -> showToUser "False" s
                            Just (TTBoolean nn)-> showToUser (show (boolToInt nn)) s
                        TString  -> case (M.lookup index mapp) of
                            Nothing -> showToUser "" s
                            Just (TTString nn) -> showToUser nn s
                        otherwise -> error("Error - multidimmensional tables are not implemented!")
                else 
                    error("Error - index out of bound!")
            TTBoolean _ -> error("Error - Variable: " ++ (show x) ++ " is not an array (B)!")
            TTString _ -> error("Error - Variable: " ++ (show x) ++ " is not an array (s)!")
            TTInt _ -> error("Error - Variable: " ++ (show x) ++ " is not an array (i)!")
            otherwise -> error("Error - Variable: " ++ (show x) ++ " is not an array!")


    SPrintExp exp -> 
	showToUser (show (interpretExp exp s)) s
    SPrintBExp bexp -> case (interpretBExp bexp s) of
        True -> showToUser "True" s
        False -> showToUser "False" s
    SPrintCharLit str -> (showToUser [str] s)


    SPrintFun (Ident x) -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
            let globals = M.intersection extState tStateOld
            in
            case varDeclarationLine of
                NonEmptyArgs _ -> error("Error - function/procedure need argument")
                EmptyArgs -> case tTypes of
                    TTVoid -> error("Error - function must return sth...")
                    otherwise ->
                        let stateAfterFunctionCall = (interpretStmt stmt ((M.union tStateOld extState) , funcMap))
                        in
                        showToUser (identToString (Ident x) stateAfterFunctionCall) ( M.union (M.intersection (fst stateAfterFunctionCall) globals) extState, funcMap)
--                        error("Not implemented yet!")


--	SPrintFun (Ident x) -> case (M.lookup x funcMap) of
--	    Nothing -> error("Error - funciton: " ++ (show x) ++ " does not exist!")
--	    Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
--            let globals = M.intersection extState tStateOld
--            in
--            case varDeclarationLine of
--               EmptyArgs -> case tTypes of
--                    TTVoid -> error("Error - function must return sth...")
--                    otherwise -> let stateAfterFunctionCall = (interpretStmt stmt ((M.union tStateOld extState) , funcMap))
--                    in
--                    showToUser (identToString (Ident x) stateAfterFunctionCall) ( M.union (M.intersection (fst stateAfterFunctionCall) globals) extState, funcMap)
--               NonEmptyArgs _ -> error("Error - function/procedure needs arguemnt")

    SProcCall (Ident x) -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) -> 
            let globals = M.intersection extState tStateOld
            in
            case varDeclarationLine of
               EmptyArgs -> ( M.union (M.intersection (fst (interpretStmt stmt ((M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
               NonEmptyArgs _ -> error("Error - function/procedure needs arguemnt")
    SProcCallInteger (Ident x) int -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) -> 
            let globals = M.intersection extState tStateOld
            in
	        case varDeclarationLine of
	            NonEmptyArgs v -> case v of
	                DLList identList@((Ident ident):_) typee -> ( M.union (M.intersection (fst (interpretStmt stmt (M.insert ident (TTInt int) (M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
	            EmptyArgs -> error ("Error - arguments were given!")

    SProcCallExp (Ident x) exp -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
            let globals = M.intersection extState tStateOld
            in
	        case varDeclarationLine of
	            NonEmptyArgs v -> case v of
	                DLList identList@((Ident ident):_) typee -> case typee of
	                    TInt -> ( M.union (M.intersection (fst (interpretStmt stmt (M.insert ident (TTInt (interpretExp exp s)) (M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
	                    otherwise -> error("Error - incorrect type!")
	            EmptyArgs -> error ("Error - arguments were given!")


    SProcCallBExp (Ident x) bexp -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
            let globals = M.intersection extState tStateOld
            in
	        case varDeclarationLine of
	            NonEmptyArgs v -> case v of
	                DLList identList@((Ident ident):_) typee -> case typee of
	                    TBool -> ( M.union (M.intersection (fst (interpretStmt stmt (M.insert ident (TTBoolean (interpretBExp bexp s)) (M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
	                    otherwise -> error("Error - incorrect type")
	            EmptyArgs -> error ("Error - arguments were given!")


    SProcCallString (Ident x) strstr -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
            let globals = M.intersection extState tStateOld
            in
	        case varDeclarationLine of
	            NonEmptyArgs v -> case v of
	                DLList identList@((Ident ident):_) typee -> case typee of
	                    TString -> ( M.union (M.intersection (fst (interpretStmt stmt (M.insert ident (TTString strstr) (M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
	                    otherwise -> error("Error - incorrect type")
	            EmptyArgs -> error ("Error - arguments were given!")



    SProcCallId (Ident x) (Ident argIdent) -> case (M.lookup x funcMap) of
        Nothing -> error("Error - Functin/procedure: "++ (show x)++" has not been found!")
        Just (stmt, varDeclarationLine, tTypes, tStateOld) ->
            let globals = M.intersection extState tStateOld
            in
	        case varDeclarationLine of
	            NonEmptyArgs v -> case v of
	                DLList identList@((Ident ident):_) typee -> case (M.lookup argIdent extState) of
	                    Nothing     -> error("Error - variable has not been inicialized!")
	                    Just vvvv   -> if typeCheck vvvv typee then
--	                        case typee of
	                            ( M.union (M.intersection (fst (interpretStmt stmt (M.insert ident vvvv (M.union tStateOld extState) , funcMap))) globals) extState, funcMap)
	                         else
	                            error("Error - incorrect types!")
--	                        otherwise -> error("Error - incorrect type")
	            EmptyArgs -> error ("Error - arguments were given!")

--type TFuncDef = (Stmt, VarDeclarationLine, TTypes, TStateOld)
--type TFuncMap = M.Map String TFuncDef

--type TState2 = (TLoc, TEnv, TFuncMap)

changeStateToSecond :: TStateOld -> TStateOld -> TStateOld
changeStateToSecond _ a = a

simpleAddOneVar :: Ident -> TTypes -> TState3 -> TState3
--simpleAddOneVar (Ident x) value (loc, env, funcMap) = ((M.insert loc), env, funcMap) 
simpleAddOneVar (Ident x) value (state, funcMap) = ((M.insert x value state), funcMap)

-------------------BEGINNING, DECLARATIONS, ...---------------
addOneVariable :: Ident -> Type -> TState3 -> TState3
addOneVariable (Ident ident) typee state = case typee of
		TInt -> simpleAddOneVar (Ident ident) (TTInt 0) state
		TBool -> simpleAddOneVar (Ident ident) (TTBoolean False) state
		TString -> simpleAddOneVar (Ident ident) (TTString "") state
		TArray minn maxx typee -> 
			if (minn < maxx) && (minn >= 0) then 
				simpleAddOneVar (Ident ident) (TTArray minn maxx typee M.empty) state
			else 
				error("Error - incorrect table index range!")	

--addOneVariable (Ident ident) typee state = case typee of
--		TInt -> M.insert ident (TTInt 0) state
--		TBool -> M.insert ident (TTBoolean False) state
--		TString -> M.insert ident (TTString "") state
--		TArray minn maxx typee -> 
--			if (minn < maxx) && (minn >= 0) then 
--				M.insert ident (TTArray minn maxx typee M.empty) state
--			else 
--				error("Error - incorrect table index range!")	



addManyVariables :: [Ident] -> Type -> TState3 -> TState3
addManyVariables [] typee state = state 
addManyVariables ((Ident ident):tl) typee state = 
	addManyVariables tl typee (addOneVariable (Ident ident) typee state)

declareNewVariables :: VariableDeclaration -> TState3 -> TState3
declareNewVariables vars state = case vars of
	VBDoesntExists -> state
	VBExists [] -> state
	VBExists listOfVarDecl@((DLList idents typee):tl) -> 
		let s = addManyVariables idents typee state
		in
			declareNewVariables (VBExists tl) s

--type TFuncDef = (Stmt, VarDeclarationLine, TTypes, TStateOld)
--type TFuncMap = M.Map String TFuncDef
addOneProc :: ProcDeclLine -> TFuncMap -> TFuncMap
addOneProc h funcMap = case h of
    PLineNonArg (Ident x) varDecls stmt  -> M.insert x (stmt, EmptyArgs, TTVoid, (fst (declareNewVariables varDecls (M.empty, M.empty)))) funcMap 
    PLineArg	(Ident x) args varDecls stmt  -> M.insert x (stmt, (NonEmptyArgs args), TTVoid, (fst (declareNewVariables varDecls (M.empty, M.empty)))) funcMap 

addOneFunction :: FuncDeclLine -> TFuncMap -> TFuncMap
addOneFunction h funcMap = case h of
    FLineNonArg (Ident x) typee varDecls stmt  -> case varDecls of
        VBDoesntExists ->
            M.insert x (stmt, EmptyArgs, (TTInt 0), (fst (declareNewVariables (VBExists ((DLList ((Ident x):[]) typee):[])) (M.empty, M.empty)))) funcMap
        VBExists listOfVarDecl ->
            M.insert x (stmt, EmptyArgs, TTVoid, (fst (declareNewVariables varDecls (M.empty, M.empty)))) funcMap
    FLineArg	(Ident x) args typee varDecls stmt  -> M.insert x (stmt, (NonEmptyArgs args), (TTInt 0), (fst (declareNewVariables varDecls (M.empty, M.empty)))) funcMap


prepareFunctions :: ProcDeclaration -> TState3 -> TState3
prepareFunctions funs state@(s, funcMap) = case funs of
    PDoesntExist -> state
    PExists [] -> state
    FExists [] -> state
    PFExists [] [] -> state
    PExists listOfProcDecl@(h:tl) ->
            prepareFunctions (PExists tl) (s, addOneProc h funcMap)
    FExists listOfFuncDecl@(h:tl) ->
            prepareFunctions (FExists tl) (s, addOneFunction h funcMap)
    PFExists [] listOfFuncDecl@(hf:tlf) ->
            prepareFunctions (PFExists [] tlf) (s, addOneFunction hf funcMap)
    PFExists listOfProcDecl@(hp:tlp) listOfFuncDecl@(hf:tlf) ->
            prepareFunctions (PFExists tlp listOfFuncDecl) (s, addOneProc hp funcMap)

-------------INTERPRET FILE------------
interpretFile :: Program -> TState3
--interpretFile (Programm programNameHeader (Blockk variableDeclaration stmts)) = interpretStmt stmts (declareNewVariables variableDeclaration (M.empty, M.empty, M.empty))
interpretFile (Programm programNameHeader (Blockk variableDeclaration procDecl stmts)) = interpretStmt stmts (prepareFunctions procDecl (declareNewVariables variableDeclaration (M.empty, M.empty)))
--interpretFile :: Program -> TState
