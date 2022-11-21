type operator = Add | Sub | Mul | Div | Mod | Eq | Less | Greater | Geq | Leq | Neq | And | Or | Not

type program =
    Expr of expr

and expr =
    InfixOp of expr * operator * expr
  | CondExp of expr * expr * expr
  | Assign of string * expr * expr 
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | CharLit of char
  | ParenExp of expr


