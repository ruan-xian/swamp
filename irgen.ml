(* IR generation: translate takes a semantically checked AST and produces
   LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/ http://llvm.moe/ocaml/ *)

module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

type var_binding = A.typ * L.llvalue

type var_binding_map = var_binding StringMap.t

(* translate : Sast.program -> Llvm.module *)
let translate program =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which we will generate code *)
  let the_module = L.create_module context "Swamp" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.float_type context in
  (* Return the LLVM type for a Swamp type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type i8_t
    (* TODO: placeholder so exhaust etc. *)
    | A.List _ -> i32_t
    | A.Function (types, ret) ->
        let formal_types = Array.of_list (List.map ltype_of_typ types) in
        L.function_type (ltype_of_typ ret) formal_types
  in
  (* Create stub entry point function "main" *)
  let ftype = L.function_type i1_t (Array.of_list []) in
  let builder =
    L.builder_at_end context
      (L.entry_block (L.define_function "main" ftype the_module))
  in
  (* Construct code for an expression; return the value of the expression *)
  (* for this basic framework we dont need to pass around a builder, but i
     think for conditionals we will need to -- so build_expr needs to also
     take in a builder, and then instead of just returning the value, return
     a tuple of (value, new_builder) but i am not going to write this in just
     yet bc i am not 100% positive this is true, something something about
     how the builder updates itself?? -- alice this is your problem :P *)
  let rec build_expr ((t, e) : shrexpr) (var_table : var_binding_map) =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    | SCharLit c -> L.const_int i8_t (Char.code c)
    | SStringLit s -> L.build_global_stringptr s "tmp" builder
    | SInfixOp (e1, op, e2) ->
        let e1' = build_expr e1 var_table
        and e2' = build_expr e2 var_table in
        (* t1 == t2 bc we semanted *)
        ( match op with
        | A.Add -> (
          match e1 with
          | A.Int, _ -> L.build_sub
          | A.Float, _ -> L.build_fsub )
        | A.Sub -> (
          match e1 with
          | A.Int, _ -> L.build_sub
          | A.Float, _ -> L.build_fsub )
        | A.Mul -> (
          match e1 with
          | A.Int, _ -> L.build_mul
          | A.Float, _ -> L.build_fmul )
        | A.Div -> (
          match e1 with
          | A.Int, _ -> L.build_sdiv
          | A.Float, _ -> L.build_fdiv )
        | A.Mod -> (
          match e1 with
          | A.Int, _ -> L.build_srem
          | A.Float, _ -> L.build_frem )
        | Eq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Eq
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ueq )
        | Neq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Ne
          | A.Float, _ -> L.build_fcmp L.Fcmp.Une )
        | Less -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Slt
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ult )
        | Greater -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sgt
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ugt )
        | Geq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sge
          | A.Float, _ -> L.build_fcmp L.Fcmp.Uge )
        | Leq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sle
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ule )
        | And -> L.build_and
        | Or -> L.build_or
        (* TODO: PLACEHOLDERS *)
        | Not | UMinus | Cat | Cons | Head | Tail -> L.build_add )
          e1' e2' "tmp" builder
    | SUnaryOp (op, e1) ->
        let e1' = build_expr e1 var_table in
        ( match op with
        | UMinus -> (
          match e1 with
          | A.Int, _ -> L.build_neg
          | A.Float, _ -> L.build_fneg )
        | Not -> L.build_not )
          e1' "tmp" builder
    | SCondExp (condition, e1, e2) ->
        let cond = build_expr condition var_table
        and e1' = build_expr e1 var_table
        and e2' = build_expr e2 var_table in
        L.build_select cond e1' e2' "tmp" builder
    | SFunExp (formals, e) -> (
        let add_formals builder m f p =
          match f with
          | A.Formal (n, f_typ) -> (
            match f_typ with
            | Function (_, _) -> StringMap.add n (f_typ, p) m
            | _ ->
                L.set_value_name n p ;
                let local = L.build_alloca (ltype_of_typ f_typ) n builder in
                ignore (L.build_store p local builder) ;
                StringMap.add n (f_typ, local) m )
        in
        match t with
        | Function (formal_types, ret_type) ->
            let lformal_types =
              Array.of_list (List.map (fun t -> ltype_of_typ t) formal_types)
            in
            let ftype =
              L.function_type (ltype_of_typ ret_type) lformal_types
            in
            let f = L.define_function "fexp" ftype the_module in
            let body_bb = L.append_block context "body" f in
            let params_list = Array.to_list (L.params f) in
            let new_var_table =
              List.fold_left2
                (add_formals (L.builder_at_end context body_bb))
                var_table formals params_list
            in
            ignore
              (L.build_ret
                 (build_expr e new_var_table)
                 (L.builder_at_end context body_bb) ) ;
            f )
    | SFunApp (fexp, args) ->
        (* let f = match fexp with | _, SFunExp (_, _) -> build_expr fexp
           var_table | _, SVar id -> StringMap.find id var_table in *)
        let f = build_expr fexp var_table in
        let llargs = List.map (fun x -> build_expr x var_table) args in
        L.build_call f (Array.of_list llargs) "result" builder
    | SAssign (id, rhs, exp) ->
        let new_var_table =
          match rhs with
          | (Function (_, _) as f), _ ->
              let rhs' = build_expr rhs var_table in
              StringMap.add id (f, rhs') var_table
          | (_ as t), _ ->
              let var = L.build_alloca (ltype_of_typ t) id builder in
              let rhs' = build_expr rhs var_table in
              ignore (L.build_store rhs' var builder) ;
              StringMap.add id (t, var) var_table
        in
        build_expr exp new_var_table
    | SVar var -> (
        let v = StringMap.find var var_table in
        match v with
        | Function (_, _), llv -> llv
        | _, llv -> L.build_load llv var builder )
  in
  ignore (L.build_ret (build_expr program StringMap.empty) builder) ;
  the_module
