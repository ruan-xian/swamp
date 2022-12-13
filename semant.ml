(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program =

  (* Return a variable from our local symbol table *)
  let type_of_identifier type_map s =
    try StringMap.find s type_map
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr type_table : expr->shrexpr = function
    | InfixOp(e1, op, e2) as ex -> 
      let (t1, e1') = check_expr type_table e1
      and (t2, e2') = check_expr type_table e2 in
      if t1 = t2 then 
        (*TODO: the resulting type isn't always the same as the operands but is the same for basic arithmetic. 
          Once we introduce booleans the type of the shrexpr should be updated to match each operator. See microc's semant*)
        (t1, SInfixOp((t1, e1'), op, (t2, e2')))
      else raise (Failure("Types do not match in expression " ^ string_of_expr ex))
    | UnaryOp(op, e1) -> 
      let (t, e') = check_expr type_table e1 in 
      (t, SUnaryOp(op, (t, e')))
    | IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | ParenExp(e)-> check_expr type_table e
    | Var var -> (type_of_identifier type_table var, SVar var)
    | Assign(id, rhs, exp) ->
      let (t1, e1') = check_expr type_table rhs in
      let (t2, e2') = check_expr (StringMap.add id t1 type_table) exp in
      (t2, SAssign(id, (t1, e1'), (t2, e2')))
  in
  match program with
  | Expr(e) -> check_expr StringMap.empty e
