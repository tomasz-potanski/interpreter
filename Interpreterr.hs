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

type TState = M.Map String Integer
-- nazwa zmiennej -> wartosc

--import Control.Monad.TState
--import Control.Monad.Error

interpretExp :: Exp -> TState -> Integer
interpretExp x s = case x of
  EAdd exp0 exp  -> (interpretExp exp0 s) + (interpretExp exp s)
  ESub exp0 exp  -> (interpretExp exp0 s) - (interpretExp exp s)
  EMul exp0 exp  -> (interpretExp exp0 s) * (interpretExp exp s)
  EDiv exp0 exp  -> (interpretExp exp0 s) `div` (interpretExp exp s)
  EInt n  -> n
  EId (Ident x) -> case M.lookup x s of
	Just n 	-> n
	Nothing	-> 0 -- !!rzuc blad

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
		NE. -> (interpretExp exp1 s) != (interpretExp exp2 s)
		
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
    SPrint a -> case a of
	PAId	->	PutStrLn a
	PALitVal ->	PutStrLn a

interpretFile :: Stmt -> TState
interpretFile i = interpretStmt i M.empty
--interpretFile :: Program -> TState