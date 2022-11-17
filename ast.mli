type operator = Add | Sub | Mul | Div | Mod

type program =
  Expr of expr

and expr =
  ArgExp of aexp

and aexp = (* atomic expression *)
    Var of string
  | IntLit of int
  | Assign of string * aexp * aexp
  | InfixOp of aexp * operator * aexp
