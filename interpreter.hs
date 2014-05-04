module Main where

import System.IO
import System.Environment ( getArgs, getProgName )

import Lexactualgrammar
import Paractualgrammar
import Absactualgrammar
import Skelactualgrammar
import Printactualgrammar
import ErrM

import Interpreterr
import qualified Data.Map as M



--main = do
--  interact calc
--  putStrLn ""

--calc s = 
--  let Ok e = pExp (myLexer s) 
--  in show (interpretFile e)




main :: IO()
main = do
    (filePath:_) <- getArgs
    fileHandler <- openFile filePath ReadMode
    fileContent <- hGetContents fileHandler
    case pStmt (myLexer fileContent) of
		-- ogarnac wypisywanie bledow na stdErr
        Bad s -> do 
		 hPutStrLn stderr "Error"
		 hPutStrLn stderr s
        Ok i -> putStrLn (show (M.toList (interpretFile i)))
--        Ok i -> return()
