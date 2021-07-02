module Backend.Js.Codegen
  ( codegen
  ) where

import Frontend.Syntax

import Data.List
import System.IO
import System.Environment
import System.Directory

codegen :: [Expr] -> IO String
codegen = output . concat . (map handleExpr)

output :: String -> IO String
output contents = do
  createDirectoryIfMissing False "./out"
  writeFile "./out/output.js" contents
  return "./out/output.js"

handleExpr :: Expr -> String
handleExpr c =
  case c of

    Float n -> show n

    Function name es e ->
      printFunction name es e

    BinOp op e1 e2 ->
      "(" ++ handleExpr e1 ++ " " ++
      showOp op ++ " " ++
      handleExpr e2 ++ ")"

    Var name ->
      name

    Call name es ->
      printCall name es

    StringLiteral s ->
      printString s

    Const s es ->
      printConst s es

showOp :: Op -> String
showOp Plus = "+"
showOp Minus = "-"

printFunction :: Name -> [Expr] -> Expr -> String
printFunction name es e =
  let args = intercalate ", " $ map handleExpr es
  in "function " ++ name ++ "(" ++ args ++ ") " ++
    "{\n\treturn " ++ handleExpr e ++ "\n}\n"

printString :: String -> String
printString s = "\"" ++ s ++ "\""

printConst :: Name -> Expr -> String
printConst name es = "const " ++ name ++ " = " ++ handleExpr es ++ ";\n"

functionKeywords :: Name -> Name
functionKeywords "print" = "console.log"
functionKeywords n = n

printCall :: Name -> [Expr] -> String
printCall name es =
  let args = intercalate ", " $ map handleExpr es
  in functionKeywords name ++ "(" ++ args ++ ")"
