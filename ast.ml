type operator = Add | Sub | Mul | Div | Mod | Eq | Less | Greater | Geq | Leq | Neq

type program =
    Expr of expr

and expr =
    InfixOp of expr * operator * expr
  | CondExp of expr * expr * expr
  | Assign of string * expr * expr 
  | Var of string
  | IntLit of int
  | ParenExp of expr
  | FunExp of string list * expr
  | FunAssign of string * string list * expr * expr 
  | FunApp of string * expr list 
  | FunExpApp of expr * expr list

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

let rec string_of_formals = function 
    [] -> ""
  | [f] -> f
  | hd :: tl -> hd ^ ", " ^ string_of_formals tl

let rec string_of_expr = function 
  | Var(s) -> s
  | IntLit(l) -> string_of_int l 
  | Assign(s, e1, e2) -> "let " ^ s ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | CondExp (e1, e2, e3)-> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | InfixOp(i1, op, i2) -> string_of_expr i1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr i2
  | ParenExp(e) ->  "(" ^ string_of_expr e ^ ")"
  | FunExp(fs, e) -> "fun(" ^ string_of_formals fs ^ ") -> " ^ string_of_expr e
  | FunAssign(s, fs, e1, e2) -> "let " ^ s ^ "(" ^ string_of_formals fs ^ ") = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | FunApp(s, es) -> s ^ "(" ^ string_of_args es ^ ")"
  | FunExpApp(e1, es) -> "(" ^ string_of_expr e1 ^ ")(" ^ string_of_args es ^ ")"

and string_of_args = function 
  [] -> ""
| [e] -> string_of_expr e
| hd :: tl -> string_of_expr hd ^ ", " ^ string_of_args tl

  let string_of_prog = function 
  Expr(e) -> "Parsed program: \n\t" ^ string_of_expr e 