module Frontend.Syntax where

type Name = String

data Expr
  = Float Double
  | BinOp Op Expr Expr
  | StringLiteral String
  | Call Name [Expr]
  | Var Name
  | Const Name Expr
  | Function Name [Expr] [Expr] Expr
  -- ^ The second list of expressions is only to allow
  -- setting variables inside a function.
  deriving (Eq, Ord, Show)
  
data Op
  = Plus
  | Minus
  deriving (Eq, Ord, Show)
