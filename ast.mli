type operator = Add | Sub | Mul | Div | Mod | Eq | Less | Greater | Geq | Leq | Neq

type program =
    Expr of expr

and expr =
    InfixOp of lexp * operator * expr
  | LeftExp of lexp

and lexp =
    CondExp of expr * expr * expr
  | Assign of string * expr * expr 
  | ArgExp of aexp

and aexp = (* atomic expression *)
    Var of string
  | IntLit of int
  | ParenExp of expr


  
