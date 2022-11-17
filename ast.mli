type operator = Add | Sub | Mul | Div | Mod

type program =
    Expr of expr

and expr =
    CondExp of expr * expr * expr
  | InfixExp of infixexp

and infixexp =
    ArgExp of aexp
  | InfixOp of infixexp * operator * infixexp

and aexp = (* atomic expression *)
    Var of string
  | IntLit of int
  | Assign of string * aexp * aexp

  