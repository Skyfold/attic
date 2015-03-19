{
module Parser where
import Syntax
import qualified Token as T
}

%name atticParser
%tokentype { T.Token }
%error { error . show }

%token
    var { T.Variable $$ }
    ':=' { T.Assignment }
    num { T.Number $$ }
    '+' { T.Plus }
    '-' { T.Minus }
    '*' { T.Multi }
    '/' { T.Div }
    '<' { T.LessThan }
    '==' { T.EqualTo }
    '(' { T.LeftParen }
    ')' { T.RightParen }
    '{' { T.LeftBrace }
    '}' { T.RightBrace }
    ';' { T.Semi }
    'if' { T.If }
    'while' { T.While }
    'print' { T.Print }
%%

Prog : Stmt { [$1] }
     | Stmt Prog { $1 : $2 }

Stmt : var ':=' Expr ';' { Assign $1 $3 }
     | 'print' Expr ';' { Print $2 }
     | 'if' Expr '{' Prog '}' '{' Prog '}' { If $2 $4 $7 }
     | 'while' Expr '{' Prog '}' { While $2 $4 }

Expr : Expr2 '==' Expr { (BinOp EqualTo $1 $3) }
     | Expr2 '<' Expr  { (BinOp LessThan $1 $3) }
     | Expr2 { $1 }

Expr2 : Expr3 '+' Expr2  { (BinOp Add $1 $3) }
      | Expr3 '-' Expr2  { (BinOp Sub $1 $3) }
      | Expr3 { $1 }

Expr3 : Expr4 '*' Expr3  { (BinOp Multi $1 $3) }
      | Expr4 '/' Expr3  { (BinOp Div $1 $3) }
      | Expr4 { $1 }

Expr4 : var { Var $1 }
      | num { Constant $1 }
      | '(' Expr ')' { $2 }

