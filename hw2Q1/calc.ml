open Ast

module StringMap = Map.Make(String)
let symbol_table = ref StringMap.empty

let rec eval = function 
    Lit(x)            -> x
  | Var(s)            -> StringMap.find s !symbol_table
  | Binop(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
        Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)
  | Seq(e1, e2) ->
      ignore (eval e1);
      eval e2
  | Asn(s, e) ->
      let value = eval e in
      symbol_table := StringMap.add s value !symbol_table;
      value

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
