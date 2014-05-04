module Main where

import Lexactualgrammar
import Paractualgrammar
import Absactualgrammar
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok e = pProgram (myLexer s) 
  in show (interpret e)
