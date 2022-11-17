type operator = Add | Sub | Mul | Div | Mod

type expr =
  Infix of infixexp

and infixexp = 
  InfixOp of infixexp * operator * infixexp
  | FunctionApp of fexp

and fexp = (* function application *)
    ArgExp of aexp

and aexp = (* atomic expression *)
    Var of string
  | IntLit of int
  | ParenExp of expr
  | Assign of string * expr * infixexp
