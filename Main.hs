module Main where 

import Lexer 
import Parser 
import TypeChecker 
import Interpreter 

main = getContents >>= print . parser . lexer 
