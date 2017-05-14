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

myLLexer = myLexer

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: ParseFun Program -> String ->  IO()
run p s = let ts = myLLexer s in case p ts of
    Bad err -> do
        putStrLn err
        exitFailure
    Ok tree -> runProgram tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run pProgram
    fs -> mapM_ (runFile pProgram) fs