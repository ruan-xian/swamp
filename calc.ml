open Ast

module StringMap = Map.Make(String)

(* type typ = Int | Bool | Float | String | Char *)
type typ = Int of int| Bool of bool| Float of float| String of string| Char of char

let rec eval_all = function
  Expr(x) -> 
    let symbol_table = StringMap.empty in eval symbol_table x

(* : typ *)
and eval symbol_table : typ = function
      CondExp(cond, e1, e2) -> 
        (match eval symbol_table cond with 
          0 -> eval symbol_table e2
        | _ -> eval symbol_table e1)
    | Assign(id, exp, exp2) -> 
        let value = eval symbol_table exp in
          let new_symbol_table = StringMap.add id value symbol_table in
            eval new_symbol_table exp2
    | InfixOp(e1, op, e2) -> 
        let v1  = eval symbol_table e1 in
        let v2 = eval symbol_table e2 in
        (match op with
          Add ->  match ((eval symbol_table v1, eval symbol_table v2)) with 
            (Int a, Int b) -> Int (v1 + v2)
            | (Float a, Float b) -> Float (v1 +. v2)
        | Sub -> match ((eval symbol_table v1, eval symbol_table v2)) with 
            (Int a, Int b) -> Int (v1 - v2)
            | (Float a, Float b) -> Float (v1 -. v2)
        | Mul -> match ((eval symbol_table v1, eval symbol_table v2)) with 
            (Int a, Int b) -> Int (v1 * v2)
            | (Float a, Float b) -> Float (v1 *. v2)
        | Div -> match ((eval symbol_table v1, eval symbol_table v2)) with 
            (Int a, Int b) -> Int (v1 / v2)
          | (Float a, Float b) -> Float (v1 /. v2)
        | Mod -> v1 mod v2
        | Eq -> if v1 = v2 then Bool true else Bool false
        | Neq -> if v1 != v2ye then Bool true else Bool false
        | Greater -> if v1 > v2 then Bool true else Bool false
        | Geq -> if v1 >= v2 then Bool true else Bool false
        | Less -> if v1 < v2 then Bool true else Bool false
        | Leq -> if v1 <= v2 then Bool true else Bool false
        | And -> match (v1, v2) with 
          (true, true) -> Bool true
          | (true, false) -> Bool false
          | (false, true) -> Bool false
          | (false, false) -> Bool false
        | Or -> match (v1, v2) with 
          (true, true) -> Bool true
          | (true, false) -> Bool true
          | (false, true) -> Bool true
          | (false, false) -> Bool false
        )
    | UnaryOp(op, e1) -> 
      let v1  = eval symbol_table e1 in
      (match op with 
        Not -> match v1 with 
          true -> Bool false
          | false -> Bool true
      | UMinus -> match (eval v1) with 
        Int a -> Int -v1
      | Float a -> Float -.v1)
    | IntLit(x) -> Int x
    | FloatLit(x) -> Float x
    | StringLit(x) -> String x
    | CharLit(x) -> Char x
    | Var(s) ->  StringMap.find s symbol_table

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.tokenize lexbuf in
  let result = eval_all expr in
  print_endline (string_of_int result)