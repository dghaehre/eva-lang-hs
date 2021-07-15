module Backend.Js.Codegen
  ( codegen,
  )
where

import Data.List
import Frontend.Syntax
import System.Directory
import System.Environment
import System.IO

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
    Function name es consts e ->
      printFunction name es consts e
    BinOp op e1 e2 ->
      "(" ++ handleExpr e1 ++ " "
        ++ showOp op
        ++ " "
        ++ handleExpr e2
        ++ ")"
    Var name ->
      name
    Call name es ->
      printCall name es
    PipeCalls ex names ->
      printFromPipes ex names
    StringLiteral s ->
      printString s
    Const s es ->
      printConst s es

showOp :: Op -> String
showOp Plus = "+"
showOp Minus = "-"
showOp Multiply = "*"
showOp Divide = "/"

printFunction :: Name -> [Expr] -> [Expr] -> Expr -> String
printFunction name es consts e =
  let args = intercalate ", " $ map handleExpr es
      c = concat $ map handleExpr consts
   in "function " ++ name ++ "(" ++ args ++ ") "
        ++ "{\n\t"
        ++ c
        ++ "\treturn "
        ++ handleExpr e
        ++ "\n}\n"

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

printFromPipes :: Expr -> [Name] -> String
printFromPipes ex names =
  let start = handleExpr ex
   in foldl concat' start names
  where
    concat' acc name =
      functionKeywords name ++ "(" ++ acc ++ ")"
