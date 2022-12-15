type operator = Add | Sub | Mul | Div | Mod 
              | Eq | Less | Greater | Geq | Leq | Neq 
              | And | Or | Not | UMinus
              | Cat | Cons | Head | Tail
              
type typ = 
    Int 
  | Float 
  | Char 
  | String 
  | Bool 
  | List of typ 
  | Function of typ list * typ
  | Unknown

type program =
    Expr of expr

and expr =
    InfixOp of expr * operator * expr
  | UnaryOp of operator * expr
  | CondExp of expr * expr * expr
  | Assign of string * expr * expr 
  | AssignRec of string * expr * expr 
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | CharLit of char
  | ParenExp of expr
  | ListExp of expr list
  | EmptyList of typ 
  | ListComp of expr * qual list
  | FunExp of formal list * expr
  | FunApp of expr * expr list 

and formal = Formal of string * typ

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
  | And -> "and"
  | Or -> "or"
  | Not -> "not"
  | UMinus -> "-"
  | Cat -> "@"
  | Cons -> "::"
  | Head -> "head"
  | Tail -> "tail"


let rec string_of_list stringify = function
[] -> ""
| [x] -> stringify x
| hd :: tl -> stringify hd ^ ", " ^ string_of_list stringify tl

let rec string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Char -> "char"
  | String -> "string"
  | Bool -> "bool"
  | List(typ) -> "list<" ^ string_of_typ typ ^ ">"
  | Function(params, ret) -> "(" ^ string_of_list string_of_typ params ^ " -> " ^ string_of_typ ret ^ ")"
  | Unknown -> raise (Failure("Type inference failed somewhere, this shouldn't be reached"))

let string_of_formal = function
  Formal(id, typ) -> id ^ ":" ^ string_of_typ typ

let rec string_of_expr = function 
  | Var(s) -> s
  | IntLit(l) -> string_of_int l 
  | StringLit(l) -> l 
  | CharLit(l) -> String.make 1 l 
  | FloatLit(l) -> string_of_float l
  | BoolLit(l) -> string_of_bool l
  | Assign(s, e1, e2) -> "let " ^ s ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | AssignRec(s, e1, e2) -> "let onion " ^ s ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | CondExp (e1, e2, e3)-> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | InfixOp(i1, op, i2) -> string_of_expr i1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr i2
  | UnaryOp(op, i1) ->string_of_op op ^ " " ^ string_of_expr i1
  | ParenExp(e) ->  "(" ^ string_of_expr e ^ ")"
  | EmptyList(t) -> "[" ^ string_of_typ t ^ "]"
  | ListExp(el) -> "[" ^ String.concat ";" (List.map string_of_expr el) ^ "]" 
  | ListComp(e1, ql) -> "[" ^ string_of_expr e1 ^ " " ^ String.concat " " (List.map string_of_qual ql) ^ "]"
  | FunExp(fs, e) -> "fun(" ^ string_of_list string_of_formal fs ^ ") -> " ^ string_of_expr e
  | FunApp(s, es) -> string_of_expr s ^ "(" ^ string_of_args es ^ ")"

and string_of_qual = function
  | CompFor(s, itr) -> "for " ^ s ^ " in " ^ string_of_expr itr 
  | CompIf(e) -> "if " ^ string_of_expr e
                   
and string_of_args = function 
  [] -> ""
| [e] -> string_of_expr e
| hd :: tl -> string_of_expr hd ^ ", " ^ string_of_args tl

let string_of_prog = function 
  Expr(e) -> "Parsed program: \n\t" ^ string_of_expr e 
