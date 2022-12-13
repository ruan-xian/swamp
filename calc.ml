open Ast

module StringMap = Map.Make(String)

(* type typ = Int | Bool | Float | String | Char *)
type typ = Int of int| Bool of bool| Float of float| String of string| Char of char

let rec eval_all = function
  Expr(x) -> 
    let symbol_table = StringMap.empty in eval symbol_table x

(* : typ *)
and eval symbol_table : expr->typ = function
      CondExp(cond, e1, e2) -> 
        (match eval symbol_table cond with 
          Bool(false) -> eval symbol_table e2
        | Bool(true) -> eval symbol_table e1
        | Int _ | Float _ | String _ | Char _ -> failwith "condition in conditional expression must be boolean")

    (* for now don't deal with function expressions *)
    | Assign(id, exp, exp2) -> 
        let value = eval symbol_table exp in
          let new_symbol_table = StringMap.add id value symbol_table in
            eval new_symbol_table exp2
    | InfixOp(e1, op, e2) -> 
        let v1  = eval symbol_table e1 in
        let v2 = eval symbol_table e2 in
        (match op with
          Add -> (match ((v1, v2)) with 
            (Int a, Int b) -> Int (a + b)
            | (Float a, Float b) -> Float (a +. b)
            | _ -> failwith "+ not defined for these types")
        | Sub -> (match ((v1, v2)) with 
            (Int a, Int b) -> Int (a - b)
            | (Float a, Float b) -> Float (a -. b)
            | _ -> failwith "- not defined for these types")
        | Mul -> (match ((v1, v2)) with 
            (Int a, Int b) -> Int (a * b)
            | (Float a, Float b) -> Float (a *. b)
            | _ -> failwith "* not defined for these types")
        | Div -> (match ((v1, v2)) with 
            (Int a, Int b) -> Int (a / b)
          | (Float a, Float b) -> Float (a /. b)
          | _ -> failwith "/ not defined for these types")
        | Mod -> (match (v1,v2) with (Int a, Int b) -> Int (a mod b) 
                  | _ -> failwith "% not defined for these types")
        | Eq -> if v1 = v2 then Bool true else Bool false
        | Neq -> if v1 != v2 then Bool true else Bool false
        | Greater -> if v1 > v2 then Bool true else Bool false
        | Geq -> if v1 >= v2 then Bool true else Bool false
        | Less -> if v1 < v2 then Bool true else Bool false
        | Leq -> if v1 <= v2 then Bool true else Bool false
        | And -> (match (v1, v2) with 
          (Bool a, Bool b) -> Bool (a && b)
          | _ -> failwith "and only defined for bools")
        | Or -> (match (v1, v2) with 
          (Bool a, Bool b) -> Bool (a || b)
          | _ -> failwith "or only defined for bools")
        | _ -> failwith "should be unreachable")
    | UnaryOp(op, e1) -> 
      let v1  = eval symbol_table e1 in
      (match op with 
        Not -> (match v1 with 
          Bool true -> Bool false
          | Bool false -> Bool true
          | _ -> failwith "not only defined for booleans")
      | UMinus -> (match v1 with 
          Int a -> Int(-a)
        | Float a -> Float(-.a)
        | _ -> failwith "unary minus only defined for numeric types")
      | _ -> failwith "this unary operator is not defined")
    | IntLit(x) -> Int x
    | FloatLit(x) -> Float x
    | StringLit(x) -> String x
    | CharLit(x) -> Char x
    | BoolLit(x) -> Bool x
    | ParenExp(e)-> eval symbol_table e
    | Var(s) ->  StringMap.find s symbol_table

    (* this is not the correct behavior for functions
       but leaving as placeholder *)
    | FunExp(_,_) 
    | AssignRec(_,_,_) 
    | FunApp(_,_) 
    | ListExp(_) -> failwith "functions not semantically checked yet"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.tokenize lexbuf in
  let result = eval_all expr in
  print_endline (
    match result with
      Int x -> string_of_int x
    | Float x -> string_of_float x
    | Bool x -> string_of_bool x
    | String x -> x
    | Char x -> String.make 1 x)
