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

type TState = M.Map String Integer
-- nazwa zmiennej -> wartosc

--import Control.Monad.TState
--import Control.Monad.Error

putIO :: String -> IO ()
putIO msg = do
	putStrLn msg

putError :: String -> IO ()
putError msg = do
	hPutStrLn stderr msg

showToUser :: String -> a -> a
showToUser string expr = unsafePerformIO $ do
	putIO string
	return expr

showError :: String -> a -> a
showError string expr = unsafePerformIO $ do
	putError string
	return expr

interpretExp :: Exp -> TState -> Integer
interpretExp x s = case x of
  EAdd exp0 exp  -> (interpretExp exp0 s) + (interpretExp exp s)
  ESub exp0 exp  -> (interpretExp exp0 s) - (interpretExp exp s)
  EMul exp0 exp  -> (interpretExp exp0 s) * (interpretExp exp s)
  EDiv exp0 exp  -> (interpretExp exp0 s) `div` (interpretExp exp s)
  EInt n  -> n
  EId (Ident x) -> variableValue (Ident x) s

--  EId (Ident x) -> case M.lookup x s of
--	Just n 	-> n
--	Nothing	-> 0 -- !!rzuc blad

variableValue :: Ident -> TState -> Integer
variableValue (Ident x) s = case M.lookup x s of
	Just n 	-> n
	Nothing	-> showError ("Zmienna: " ++ (show x) ++ " nie istnieje!") 0 -- !!rzuc blad

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
		
interpretStmt :: Stmt -> TState -> TState
interpretStmt stmt s = case stmt of
    SAss (Ident x) exp ->
        let val = (interpretExp exp s)
        in M.insert x val s
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
    SPrintId (Ident x) -> showToUser (show (variableValue (Ident x) s)) s
    SPrint a -> case a of
		LiteralValueString ss -> showToUser a s
		LiteralValueInteger ii -> showToUser (show ii) s  




interpretFile :: Stmt -> TState
interpretFile i = interpretStmt i M.empty
--interpretFile :: Program -> TState