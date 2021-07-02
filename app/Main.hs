module Main where

import Lib

import System.IO
import System.Environment

repl :: IO ()
repl = do
  putStrLn "ready>"
  putStrLn "repl not ready"

processFile :: String -> IO ()
processFile filename = do
  file <- readFile filename
  process file

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [filename] -> processFile filename
