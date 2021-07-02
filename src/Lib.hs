module Lib 
    ( process
    ) where

import Frontend.Parser
import Backend.Js.Codegen

process :: String -> IO ()
process content = do
  let res = parseToplevel content
  case res of
    Left err -> print err
    Right ex -> do
      putStrLn $ show ex
      file <- codegen ex
      putStrLn file
