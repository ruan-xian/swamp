type operator = Add | Sub | Mul | Div | Mod | Eq | Less | Greater | Geq | Leq | Neq | And | Or | Not | UMinus | Cat | Cons | Head | Tail

type program =
    Expr of expr

and expr =
    InfixOp of expr * operator * expr
  | UnaryOp of operator * expr
  | CondExp of expr * expr * expr
  | Assign of string * expr * expr 
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | CharLit of char
  | ParenExp of expr
  | ListExp of expr list
  | ListComp of expr * qual list

and qual =
  | CompFor of string * expr
  | CompIf of expr

  let string_of_op = function
  Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Less -> "<"
  | Greater -> ">"
  | Geq -> ">="
  | Leq -> "<="
  | Neq -> "!="
  | Cat -> "@"
  | Cons -> "::"
  | Head -> "head"
  | Tail -> "tail"
  | And -> "and"
  | Or -> "or"
  | Not -> "not"
  | UMinus -> "-"

let rec string_of_expr = function 
  | Var(s) -> s
  | IntLit(l) -> string_of_int l 
  | StringLit(l) -> l 
  | CharLit(l) -> String.make 1 l 
  | FloatLit(l) -> string_of_float l
  | BoolLit(l) -> string_of_bool l
  | Assign(s, e1, e2) -> "let " ^ s ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | CondExp (e1, e2, e3)-> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | InfixOp(i1, op, i2) -> string_of_expr i1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr i2
  | UnaryOp(op, i1) ->string_of_op op ^ " " ^ string_of_expr i1
  | ParenExp(e) ->  "(" ^ string_of_expr e ^ ")"
  | ListExp(el) -> "[" ^ String.concat ";" (List.map string_of_expr el) ^ "]" 
  | ListComp(e1, ql) -> "[" ^ string_of_expr e1 ^ " " ^ String.concat " " (List.map string_of_qual ql) ^ "]"

and string_of_qual = function
  | CompFor(s, itr) -> "for " ^ s ^ " in " ^ string_of_expr itr 
  | CompIf(e) -> "if " ^ string_of_expr e

let string_of_prog = function 
  Expr(e) -> "Parsed program: \n\t" ^ string_of_expr e 
