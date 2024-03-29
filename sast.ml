open Ast

type shrexpr = typ * sx

and squal = typ * sq

and sx =
    SInfixOp of shrexpr * operator * shrexpr
  | SUnaryOp of operator * shrexpr
  | SCondExp of shrexpr * shrexpr * shrexpr
  | SAssign of string * shrexpr * shrexpr 
  | SAssignRec of string * shrexpr * shrexpr 
  | SVar of string
  | SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStringLit of string
  | SListExp of shrexpr list
  | SListComp of shrexpr * squal list
  | SFunExp of formal list * shrexpr
  | SFunApp of shrexpr * shrexpr list 
  | SUnknown

and sq =
  | SCompFor of string * shrexpr
  | SCompIf of shrexpr

(*Pretty printing*)

let rec string_of_shrexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    | SVar(s) -> s
    | SIntLit(l) -> string_of_int l 
    | SStringLit(l) -> l 
    | SFloatLit(l) -> string_of_float l
    | SBoolLit(l) -> string_of_bool l
    | SAssign(s, e1, e2) -> "let " ^ s ^ " = " ^ string_of_shrexpr e1 ^ " in " ^ string_of_shrexpr e2
    | SAssignRec(s, e1, e2) -> "let onion " ^ s ^ " = " ^ string_of_shrexpr e1 ^ " in " ^ string_of_shrexpr e2
    | SCondExp (e1, e2, e3)-> "if " ^ string_of_shrexpr e1 ^ " then " ^ string_of_shrexpr e2 ^ " else " ^ string_of_shrexpr e3
    | SInfixOp(i1, op, i2) -> string_of_shrexpr i1 ^ " " ^ string_of_op op ^ " " ^ string_of_shrexpr i2
    | SUnaryOp(op, i1) ->string_of_op op ^ " " ^ string_of_shrexpr i1
    | SListExp(el) -> "[" ^ String.concat ";" (List.map string_of_shrexpr el) ^ "]" 
    | SListComp(e1, ql) -> "[" ^ string_of_shrexpr e1 ^ " " ^ String.concat " " (List.map string_of_squal ql) ^ "]"
    | SFunExp(fs, e) -> "fun(" ^ string_of_list string_of_formal fs ^ ") -> " ^ string_of_shrexpr e
    | SFunApp(s, es) -> string_of_shrexpr s ^ "(" ^ string_of_sargs es ^ ")"
    | SUnknown -> "Unknown expression - type inference failed"
    ) ^ ")"

and string_of_squal (t, q) = 
  match q with
    SCompFor(s, itr) -> "for " ^ s ^ " in " ^ string_of_shrexpr itr 
  | SCompIf(e) -> "if " ^ string_of_shrexpr e
                   
and string_of_sargs = function 
  [] -> ""
| [e] -> string_of_shrexpr e
| hd :: tl -> string_of_shrexpr hd ^ ", " ^ string_of_sargs tl
