open Ast

module StringMap = Map.Make(String)
let symbol_table = ref StringMap.empty

let rec eval = function
  Infix(x) -> eval_infixexp x

and eval_infixexp = function
    FunctionApp(x) -> eval_fexp x
  | InfixOp(e1, op, e2) ->
    let v1  = eval_infixexp e1 in
    let v2 = eval_infixexp e2 in
    (match op with
      Add -> v1 + v2
    | Sub -> v1 - v2
    | Mul -> v1 * v2
    | Div -> v1 / v2
    | Mod -> v1 mod v2)

and eval_fexp = function 
    ArgExp(x) -> eval_aexp x

and eval_aexp = function 
    IntLit(x)            -> x
  | Var(s)            -> StringMap.find s !symbol_table
  | ParenExp(x) -> eval x
  | Assign(id, exp, exp2) -> 
    let value = eval exp in
    symbol_table := StringMap.add id value !symbol_table;
    eval_infixexp exp2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
