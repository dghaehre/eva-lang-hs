module Frontend.Syntax where

type Name = String

data Expr
  = Float Double
  | BinOp Op Expr Expr
  | StringLiteral String
  | Call Name [Expr]
  | PipeCalls Expr [Name]
  | Var Name
  | Const Name Expr
  | -- | The second list of expressions is only to allow
    -- setting variables inside a function.
    Function Name [Expr] [Expr] Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Multiply
  | Divide
  deriving (Eq, Ord, Show)
