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

getFileContent :: [String] -> String
getFileContent (filePath:_) = do
    fileHandler <- openFile filePath ReadMode
    fileContent <- hGetContents fileHandler
    return fileContent
getFileContent [] = do
	c <- getContents
	return c

-- !! WCZYTYWANIE Z WEJSCIA PRZY BRAKU PARAM.
main :: IO()
main = do
--    (filePath:_) <- getArgs
	args 	<-	getArgs
	let fileContent = getFileContent args
--    fileHandler <- openFile filePath ReadMode
--   fileContent <- hGetContents fileHandler
--    fileContent <- getContents	
    	case pProgram (myLexer fileContent) of
		-- ogarnac wypisywanie bledow na stdErr
        	Bad s -> do 
		 	hPutStrLn stderr "Error"
		 	hPutStrLn stderr s
        	Ok i -> putStrLn (show (M.toList (interpretFile i)))
--        Ok i -> return()

-- s <- getContents
