open Ast

module StringMap = Map.Make(String)

let rec eval_all = function
  Expr(x) -> 
    let symbol_table = StringMap.empty in eval symbol_table x

and eval symbol_table = function
  CondExp(cond, e1, e2) -> 
    (match eval symbol_table cond with 
      0 -> eval symbol_table e2
    | _ -> eval symbol_table e1)
  | InfixExp(x) -> eval_infixexp symbol_table x

and eval_infixexp symbol_table = function
    ArgExp(x) -> eval_aexp symbol_table x
  | InfixOp(e1, op, e2) -> 
    let v1  = eval_infixexp symbol_table e1 in
    let v2 = eval_infixexp symbol_table e2 in
    (match op with
      Add -> v1 + v2
    | Sub -> v1 - v2
    | Mul -> v1 * v2
    | Div -> v1 / v2
    | Mod -> v1 mod v2)

and eval_aexp symbol_table = function 
    IntLit(x)            -> x
  | Var(s)            -> StringMap.find s symbol_table
  | Assign(id, exp, exp2) -> 
    let value = eval_aexp symbol_table exp in
      let new_symbol_table = StringMap.add id value symbol_table in
        eval_aexp new_symbol_table exp2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.tokenize lexbuf in
  let result = eval_all expr in
  print_endline (string_of_int result)
