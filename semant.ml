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
    | InfixOp (e1, op, e2) as e -> (
        let t1, e1' = check_expr type_table e1
        and t2, e2' = check_expr type_table e2 in
        let err =
          "illegal infix operator " ^ string_of_typ t1 ^ " "
          ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in "
          ^ string_of_expr e
        in
        (* char + char *)
        (* All binary operators require operands of the same type*)
        if t1 = Unknown || t2 = Unknown then
          (* Come back to check the type after we've inferred it *)
          (Unknown, SUnknown)
        else
          (* Determine expression type based on operator and operand types *)
          match t1 = t2 with
          | true ->
              let t =
                match op with
                | Add when t1 = String -> String
                | Add when t1 = Char -> String
                | (Add | Sub | Mul | Div | Mod) when t1 = Int || t1 = Float
                  ->
                    t1
                | Eq | Neq -> Bool
                | (Less | Greater | Geq | Leq) when t1 = Int || t1 = Float ->
                    Bool
                | (And | Or) when t1 = Bool -> Bool
                | Cat -> (
                  match t1 with
                  | List typ -> List typ
                  | _ -> raise (Failure err) )
                | _ -> raise (Failure err)
              in
              (t, SInfixOp ((t1, e1'), op, (t2, e2')))
          | false
            when (t1 = Char && t2 = String) || (t1 = String && t2 = Char) ->
              (String, SInfixOp ((t1, e1'), op, (t2, e2')))
          | false when op = Cons && t2 = List t1 ->
              (t2, SInfixOp ((t1, e1'), Cons, (t2, e2')))
          | _ -> raise (Failure err) )
    | UnaryOp (op, e1) as ex -> (
        let t, e' = check_expr type_table e1 in
        if t = Unknown then (Unknown, SUnknown)
        else
          let err =
            "Invalid operand type for expression " ^ string_of_expr ex
          in
          match op with
          | UMinus when t = Int || t = Float -> (t, SUnaryOp (op, (t, e')))
          | Not when t = Bool -> (t, SUnaryOp (op, (t, e')))
          | Head -> (
            match t with
            | List typ -> (typ, SUnaryOp (op, (t, e')))
            | _ -> raise (Failure err) )
          | Tail -> (
            match t with
            | List typ -> (List typ, SUnaryOp (op, (t, e')))
            | _ -> raise (Failure err) )
          | _ -> raise (Failure err) )
    | CondExp (condition, e1, e2) as ex ->
        let t, e' = check_expr type_table condition in
        if t = Bool || t = Unknown then
          let t1, e1' = check_expr type_table e1 in
          let t2, e2' = check_expr type_table e2 in
          if t1 = t2 then (t1, SCondExp ((t, e'), (t1, e1'), (t2, e2')))
          else if t1 = Unknown then
            (t2, SCondExp ((t, e'), (t1, e1'), (t2, e2')))
          else if t2 = Unknown then
            (t1, SCondExp ((t, e'), (t1, e1'), (t2, e2')))
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
    | ParenExp e -> check_expr type_table e
    | Var var -> (type_of_identifier type_table var, SVar var)
    | EmptyList t -> (List t, SListExp [])
    | ListExp l ->
        let typed_list =
          if l = [] then raise (Failure "Cannot declare untyped empty list")
          else List.map (check_expr type_table) l
        in
        let tlst =
          if l = [] then raise (Failure "Cannot declare untyped empty list")
          else fst (List.hd typed_list)
        in
        let rec check_list lst t =
          match lst with
          | [] -> t
          | hd :: tl ->
              if t = fst hd then check_list tl t
              else if fst hd = Unknown then Unknown
              else
                raise
                  (Failure
                     ( "Inconsistent type in "
                     ^ string_of_list string_of_expr l ) )
        in
        let checked_type = check_list typed_list tlst in
        if checked_type = tlst then (List tlst, SListExp typed_list)
        else if checked_type = Unknown then
          (List Unknown, SListExp typed_list)
        else
          raise
            (Failure
               ("Inconsistent type in " ^ string_of_list string_of_expr l) )
    | ListComp (e, ql) -> (
        let check_comp m q =
          let t, e' = check_qual m q in
          match t with
          | Bool -> (m, (t, e'))
          | Unknown -> (m, (Unknown, e'))
          | _ -> (
            match e' with
            | SCompFor (id, _) -> (StringMap.add id t m, (t, e'))
            | _ -> raise (Failure "If you're seeing this, idk why") )
        in
        let comp_map, typed_qlst =
          List.fold_left_map check_comp type_table ql
        in
        let t, se = check_expr comp_map e in
        match t with
        | Unknown ->
            (List Unknown, SListComp ((Unknown, SUnknown), typed_qlst))
        | _ -> (List t, SListComp ((t, se), typed_qlst)) )
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
          List.rev
            (List.fold_left
               (fun l f -> match f with Formal (_, ty) -> ty :: l)
               [] formals )
        in
        if t = Unknown then (Unknown, SUnknown)
        else (Function (types, t), SFunExp (formals, (t, e)))
    | FunApp (func, args) as fapp -> (
        let check_func_app param_types return_type =
          let param_length = List.length param_types in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting "
                 ^ string_of_int param_length
                 ^ " arguments in " ^ string_of_expr fapp ) )
          else
            let check_call ft e =
              let et, e' = check_expr type_table e in
              if ft = et || et = Unknown then (et, e')
              else
                raise
                  (Failure
                     ( "illegal argument found " ^ string_of_typ et
                     ^ " expected " ^ string_of_typ ft ^ " in "
                     ^ string_of_expr e ) )
            in
            let args' = List.map2 check_call param_types args in
            (* Check if any arguments are Unknown *)
            if
              List.fold_left ( || ) false
                (List.map
                   (fun shrex ->
                     match shrex with Unknown, _ -> true | _ -> false )
                   args' )
              (* If at least one is unknown, then the type of this call is
                 Unknown *)
            then (Unknown, SUnknown)
            else
              let fname' = check_expr type_table func in
              (return_type, SFunApp (fname', args'))
        in
        match func with
        | Var fname -> (
          match type_of_identifier type_table fname with
          | Function (param_types, return_type) ->
              check_func_app param_types return_type
          | Unknown -> (Unknown, SUnknown)
          | _ -> raise (Failure (fname ^ " is not a function")) )
        | _ -> (
            let ftype, fexpr = check_expr type_table func in
            match ftype with
            | Function (param_types, return_type) ->
                check_func_app param_types return_type
            | _ ->
                raise
                  (Failure
                     ("This" ^ string_of_expr func ^ " is not a function") )
            ) )
    | AssignRec (id, body, exp) ->
        let t_inferred, _ =
          check_expr (StringMap.add id Unknown type_table) body
        in
        if t_inferred = Unknown then
          raise
            (Failure
               ( "Failed to infer return type of " ^ id ^ " in declaration "
               ^ string_of_expr body ) )
        else
          let t_inferred, e_body' =
            check_expr (StringMap.add id t_inferred type_table) body
          in
          let t2, e2' =
            check_expr (StringMap.add id t_inferred type_table) exp
          in
          (t2, SAssignRec (id, (t_inferred, e_body'), (t2, e2')))
  and check_qual type_table : qual -> squal = function
    | CompFor (id, e) as cf -> (
        let t, e' = check_expr type_table e in
        match t with
        | List t' -> (t', SCompFor (id, (t, e')))
        | _ ->
            raise
              (Failure
                 ( "Invalid list comprehension expression: "
                 ^ string_of_qual cf ) ) )
    | CompIf e as ci -> (
        let t, e' = check_expr type_table e in
        match t with
        | Bool -> (Bool, SCompIf (Bool, e'))
        | Unknown -> (Unknown, SCompIf (Unknown, SUnknown))
        | _ ->
            raise
              (Failure
                 ( "Invalid list comprehension expression: "
                 ^ string_of_qual ci ) ) )
  in
  let ssays_t = Function([String], Int) in
  let ttable = StringMap.add "shreksays" ssays_t StringMap.empty in
  let iint_to_string = Function([Int], String) in
  let ttable = StringMap.add "intToString" iint_to_string ttable in
  let ffloat_to_string = Function([Float], String) in
  let ttable = StringMap.add "floatToString" ffloat_to_string ttable in
  match program with Expr e -> check_expr ttable e
