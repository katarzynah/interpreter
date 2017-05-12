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


run :: String -> IO()
run s = case pProgram (myLexer s) of
    Bad err -> do
        putStrLn err
        exitFailure
    Ok tree -> transProgram tree


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= run
    [] -> hGetContents stdin >>= run