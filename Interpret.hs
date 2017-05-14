module Main where

import System.IO ( stdin, hGetContents )

import LexInterpreter
import ParInterpreter
import PrintInterpreter
import AbsInterpreter

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Interpreter

import ErrM

type ParseFun a = [Token] -> Err a

<<<<<<< HEAD
myLLexer = myLexer

=======
>>>>>>> 3ab0404bb73b04ae7fb3ec4d71ff82d36775821d
runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: ParseFun Program -> String ->  IO()
<<<<<<< HEAD
run p s = let ts = myLLexer s in case p ts of
=======
run p s = let ts = myLexer s in case p ts of
>>>>>>> 3ab0404bb73b04ae7fb3ec4d71ff82d36775821d
    Bad err -> do
        putStrLn err
        exitFailure
    Ok tree -> transProgram tree


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run pProgram
    fs -> mapM_ (runFile pProgram) fs