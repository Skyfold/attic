
{
module Token where
}

%wrapper "basic"

$digit = 0-9
$alpha = [α-ωΑ-Ω]

tokens :-

    $white+ ;
    $alpha $alpha* {Variable}
    ":=" {\s -> Assignment}
    $digit $digit* {\s -> Number (read s)}
    "+" {\s -> Plus}
    "-" {\s -> Minus}
    "*" {\s -> Multi}
    "/" {\s -> Div}
    "<" {\s -> LessThan}
    "==" {\s -> EqualTo}
    "(" {\s -> LeftParen}
    ")" {\s -> RightParen}
    "{" {\s -> LeftBrace}
    "}" {\s -> RightBrace}
    ";" {\s -> Semi}
    "if" {\s -> If}
    "while" {\s -> While}
    "print" {\s -> Print}

{
data Token = Variable String
           | Assignment
           | Number Int
           | Plus
           | Minus
           | Multi
           | Div
           | LessThan
           | EqualTo
           | LeftParen
           | RightParen
           | LeftBrace
           | RightBrace
           | Semi
           | If
           | While
           | Print
    deriving (Show)
}


