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
import Typechecker
import qualified Data.Map as M



--main = do
--  interact calc
--  putStrLn ""

--calc s = 
--  let Ok e = pExp (myLexer s) 
--  in show (interpretFile e)

--getFileContent :: [String] -> String
--getFileContent (filePath:_) = do
--    fileHandler <- openFile filePath ReadMode
--    fileContent <- hGetContents fileHandler
--    return fileContent
--getFileContent [] = do
--	c <- getContents
--	return c

isNotNull :: [a] -> Bool
isNotNull [] = False
isNotNull (h:_) = True

giveFirst :: [a] -> a
giveFirst (h:_) = h

main :: IO()
main = do
    args <- getArgs
    if (isNotNull args) then do
	let filePath = giveFirst args
    	fileHandler <- openFile filePath ReadMode
    	fileContent <- hGetContents fileHandler
    	case pProgram (myLexer fileContent) of
    	    Bad s -> do 
		 hPutStrLn stderr "Error"
		 hPutStrLn stderr s
            Ok i ->
                case (M.lookup "#ERROR" (fst (typecheckFile i))) of
                    Nothing ->
                        putStrLn (show (M.toList (fst (interpretFile i))))
                    Just vv -> error("Error - TYPECHECKER ERROR")

    else do
	fileContent <- getContents
    	case pProgram (myLexer fileContent) of
    	    Bad s -> do 
		 hPutStrLn stderr "Error"
		 hPutStrLn stderr s
--            Ok i -> return()
            Ok i -> putStrLn (show (M.toList (fst (interpretFile i))))
