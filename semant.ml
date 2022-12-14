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
        (match op with
          Add | Sub | Mul | Div | Mod ->
            if t1 = Int || t1 = Float then (t1, SInfixOp((t1, e1'), op, (t2, e2')))
            else raise (Failure("Invalid operand types for expression " ^ string_of_expr ex))
        | Less | Greater | Geq | Leq ->
            if t1 = Int || t1 = Float then (Bool, SInfixOp((t1, e1'), op, (t2, e2')))
            else raise (Failure("Invalid operand types for expression " ^ string_of_expr ex))
        | Eq | Neq -> (Bool, SInfixOp((t1, e1'), op, (t2, e2')))
        | And | Or ->
            if t1 = Bool then (Bool, SInfixOp((t1, e1'), op, (t2, e2')))
            else raise (Failure("Invalid operand types for expression " ^ string_of_expr ex))
        )
      else raise (Failure("Mismatched operand types in expression " ^ string_of_expr ex))
    | UnaryOp(op, e1) as ex -> 
      let (t, e') = check_expr type_table e1 in 
      (match op with
          UMinus -> 
            if t = Int || t = Float then (t, SUnaryOp(op, (t, e')))
            else raise (Failure("Invalid operand type for expression " ^ string_of_expr ex))
        | Not ->
            if t = Bool then (t, SUnaryOp(op, (t, e')))
            else raise (Failure("Invalid operand type for expression " ^ string_of_expr ex))
      )
    | CondExp(condition, e1, e2) as ex ->
      let (t, e') = check_expr type_table condition in 
      if t = Bool then 
        let (t1, e1') = check_expr type_table e1 in
        let (t2, e2') = check_expr type_table e2 in
        if t1 = t2 then
          (t1, SCondExp((t, e'), (t1, e1'), (t2, e2')))
        else raise (Failure("Both cases of expression " ^ string_of_expr ex ^ " must have the same type"))
      else raise (Failure("Condition in expression " ^ string_of_expr ex ^ " must be boolean"))
    | IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | ParenExp(e)-> check_expr type_table e
    | Var var -> (type_of_identifier type_table var, SVar var)
    | Assign(id, rhs, exp) ->
      let (t1, e1') = check_expr type_table rhs in
      let (t2, e2') = check_expr (StringMap.add id t1 type_table) exp in
      (t2, SAssign(id, (t1, e1'), (t2, e2')))
    | ListExp(l) -> 
      let typed_list = List.map (check_expr type_table) l in
      let rec check_list lst t =
        match lst with
          [] -> true
        | hd :: tl ->
            if ty = fst(hd) then
              check_list tl t
            else
              false
      in
      if check_list typed_list fst(List.hd typed_list) then
        (t, SListExp(typed_list))
      else raise (Failure("Inconsistent type in " ^ string_of_expr l))
  in
  match program with
  | Expr(e) -> check_expr StringMap.empty e
