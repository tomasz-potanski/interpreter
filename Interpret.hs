-----downloaded from: http://bnfc.digitalgrammars.com/tutorial.html

module Main where


import LexCalc
import ParCalc
import AbsCalc
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok e = pProgram (myLexer s) 
  in show (runProg e)
