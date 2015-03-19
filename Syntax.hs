import qualified Data.Set as S
module Syntax where

data Expression = Constant Int
                | Var String
                | BinOp Operation Expression Expression

data Operation = Add
               | Sub
               | LessThan
               | EqualTo
               | Multi
               | Div

data Stmt = Assign String Expression
          | Print Expression
          | If Expression Prog Prog
          | While Expression Prog

type Prog = [Stmt]
