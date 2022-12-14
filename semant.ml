(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful, throws an
   exception if something is wrong.

   Check each global variable, then check each function *)

let check program =
  (* Return a variable from our local symbol table *)
  let type_of_identifier type_map s =
    try StringMap.find s type_map
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  (* Return a semantically-checked expression, i.e., with a type *)

  let rec check_expr type_table : expr -> shrexpr = function
    | InfixOp (e1, op, e2) as e ->
        let t1, e1' = check_expr type_table e1
        and t2, e2' = check_expr type_table e2 in
        let err =
          "illegal infix operator " ^ string_of_typ t1 ^ " "
          ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in "
          ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t =
            match op with
              Add when t1 = String -> String
            | (Add | Sub | Mul | Div | Mod) when t1 = Int || t1 = Float -> t1
            | Eq | Neq -> Bool
            | (Less | Greater | Geq | Leq) when t1 = Int || t1 = Float ->
                Bool
            | (And | Or) when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SInfixOp ((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
    | UnaryOp (op, e1) as ex -> (
        let t, e' = check_expr type_table e1 in
        match op with
        | UMinus when t = Int || t = Float -> (t, SUnaryOp (op, (t, e')))
        | Not when t = Bool -> (t, SUnaryOp (op, (t, e')))
        | _ ->
            raise
              (Failure
                 ("Invalid operand type for expression " ^ string_of_expr ex)
              ) )
    | CondExp (condition, e1, e2) as ex ->
        let t, e' = check_expr type_table condition in
        if t = Bool then
          let t1, e1' = check_expr type_table e1 in
          let t2, e2' = check_expr type_table e2 in
          if t1 = t2 then (t1, SCondExp ((t, e'), (t1, e1'), (t2, e2')))
          else
            raise
              (Failure
                 ( "Both cases of expression " ^ string_of_expr ex
                 ^ " must have the same type" ) )
        else
          raise
            (Failure
               ( "Condition in expression " ^ string_of_expr ex
               ^ " must be boolean" ) )
    | IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StringLit l -> (String, SStringLit l)
    | CharLit l -> (Char, SCharLit l)
    | ParenExp(e)-> check_expr type_table e
    | Var var -> (type_of_identifier type_table var, SVar var)
    | ListExp(l) -> 
      let typed_list = List.map (check_expr type_table) l in
      let tlst = fst(List.hd typed_list) in
      let rec check_list lst t =
        match lst with
          [] -> true
        | hd :: tl ->
            if t = fst(hd) then
              check_list tl t
            else
              false
      in
      if check_list typed_list tlst then
        (tlst, SListExp(typed_list))
      else raise (Failure("Inconsistent type in " ^ string_of_list string_of_expr l))
    | Assign (id, rhs, exp) ->
        let t1, e1' = check_expr type_table rhs in
        let t2, e2' = check_expr (StringMap.add id t1 type_table) exp in
        (t2, SAssign (id, (t1, e1'), (t2, e2')))
    | FunExp (formals, rhs) ->
        let _ =
          List.fold_left
            (fun m f ->
              match f with
              | Formal (name, ty) when not (StringMap.mem name m) ->
                  StringMap.add name ty m
              | _ ->
                  raise
                    (Failure
                       ( "Names for formals in "
                       ^ string_of_list string_of_formal formals
                       ^ " must be unique" ) ) )
            StringMap.empty formals
        in
        let new_map =
          List.fold_left
            (fun m f ->
              match f with Formal (name, ty) -> StringMap.add name ty m )
            type_table formals
        in
        let t, e = check_expr new_map rhs in
        let types =
          List.fold_left
            (fun l f -> match f with Formal (_, ty) -> ty :: l)
            [] formals
        in
        (Function (types, t), SFunExp (formals, (t, e)))
    (* TODO *)
    | AssignRec (_, _, _)
     | ListExp _
     |ListComp (_, _)
     |FunApp (_, _) ->
        (Int, SIntLit 0)
  in
  match program with Expr e -> check_expr StringMap.empty e
