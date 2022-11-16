type operator = Add | Sub | Mul | Div | Mod

type expr =
  Infix of infixexp

and infixexp = 
  InfixOp of infixexp * operator * infixexp
  | LetExpr2 of decls * expr
  | FunctionApp of fexp

(* and lexp =
    LetExpr2 of decls * expr *)
  (* | FunctionApp of fexp *)

and decls =
    Assign of string * expr

and fexp =
    ArgExp of aexp

and aexp = 
    Var of string
  | IntLit of int
  | ParenExp of expr
