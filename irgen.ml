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
  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type i8_t
    | A.List t -> L.pointer_type (ltype_of_typ t)
    | A.Function (types, ret) ->
        let formal_types = Array.of_list (List.map ltype_of_typ types) in
        L.pointer_type (L.function_type (ltype_of_typ ret) formal_types)
    | A.Unknown -> failwith "how the fuck did you get here"
  in
  (* Struct Declarations *)

  (* struct Node {
   *    void *val;
   *    struct Node *next;
   * };
   *)
  let node_t : L.lltype = L.named_struct_type context "Node" in
  L.struct_set_body node_t
    [|L.pointer_type i8_t; L.pointer_type node_t|]
    false ;
  (* struct List {
   *    struct Node *head;
   *    int len;
   * };
   *)
  let list_t : L.lltype = L.named_struct_type context "List" in
  L.struct_set_body list_t [|L.pointer_type node_t; i32_t|] false ;
  (* Library Function Declarations *)
  let shreksays_t : L.lltype =
    L.var_arg_function_type i32_t [|L.pointer_type i8_t|]
  in
  let shreksays_f : L.llvalue =
    L.declare_function "shreksays" shreksays_t the_module
  in
  let newEmptyList_t : L.lltype =
    L.function_type (L.pointer_type list_t) [||]
  in
  let newEmptyList_f : L.llvalue =
    L.declare_function "newEmptyList" newEmptyList_t the_module
  in
  let newNode_t : L.lltype =
    L.function_type (L.pointer_type node_t) [|L.pointer_type i8_t|]
  in
  let newNode_f : L.llvalue =
    L.declare_function "newNode" newNode_t the_module
  in
  let appendNode_t : L.lltype =
    L.function_type (L.pointer_type list_t)
      [|L.pointer_type list_t; L.pointer_type node_t|]
  in
  let appendNode_f : L.llvalue =
    L.declare_function "appendNode" appendNode_t the_module
  in
  (* Create stub entry point function "main" *)
  let ftype = L.function_type i32_t (Array.of_list []) in
  let builder_init =
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
  let rec build_expr ((t, e) : shrexpr) (var_table : var_binding_map) builder
      =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    | SCharLit c -> L.const_int i8_t (Char.code c)
    | SStringLit s -> L.const_stringz context s
    | SInfixOp (e1, op, e2) ->
        let e1' = build_expr e1 var_table builder
        and e2' = build_expr e2 var_table builder in
        (* t1 == t2 bc we semanted *)
        ( match op with
        | A.Add -> (
          match e1 with
          | A.Int, _ -> L.build_add
          | A.Float, _ -> L.build_fadd
          | _ -> failwith "unreachable" )
        | A.Sub -> (
          match e1 with
          | A.Int, _ -> L.build_sub
          | A.Float, _ -> L.build_fsub
          | _ -> failwith "unreachable" )
        | A.Mul -> (
          match e1 with
          | A.Int, _ -> L.build_mul
          | A.Float, _ -> L.build_fmul
          | _ -> failwith "unreachable" )
        | A.Div -> (
          match e1 with
          | A.Int, _ -> L.build_sdiv
          | A.Float, _ -> L.build_fdiv
          | _ -> failwith "unreachable" )
        | A.Mod -> (
          match e1 with
          | A.Int, _ -> L.build_srem
          | A.Float, _ -> L.build_frem
          | _ -> failwith "unreachable" )
        | Eq -> (
          match e1 with
          | A.Int, _ | A.Bool, _-> L.build_icmp L.Icmp.Eq
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ueq
          (* quick someone do the other types or smth idk *)
          | _ -> failwith "unreachable" )
        | Neq -> (
          match e1 with
          | A.Int, _ | A.Bool, _-> L.build_icmp L.Icmp.Ne
          | A.Float, _ -> L.build_fcmp L.Fcmp.Une
          | _ -> failwith "unreachable" )
        | Less -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Slt
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ult
          | _ -> failwith "unreachable" )
        | Greater -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sgt
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ugt
          | _ -> failwith "unreachable" )
        | Geq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sge
          | A.Float, _ -> L.build_fcmp L.Fcmp.Uge
          | _ -> failwith "unreachable" )
        | Leq -> (
          match e1 with
          | A.Int, _ -> L.build_icmp L.Icmp.Sle
          | A.Float, _ -> L.build_fcmp L.Fcmp.Ule
          | _ -> failwith "unreachable" )
        | And -> L.build_and
        | Or -> L.build_or
        (* TODO: PLACEHOLDERS *)
        | UMinus | Cat | Cons | Head | Tail -> L.build_add )
          e1' e2' "tmp" builder
    | SUnaryOp (op, e1) ->
        let e1' = build_expr e1 var_table builder in
        ( match op with
        | UMinus -> (
          match e1 with
          | A.Int, _ -> L.build_neg
          | A.Float, _ -> L.build_fneg
          | _ -> failwith "unreachable" )
        | Not -> L.build_not
        | _ -> failwith "unreachable" )
          e1' "tmp" builder
    | SCondExp (condition, e1, e2) ->
        let cond = build_expr condition var_table builder
        and e1' = build_expr e1 var_table builder
        and e2' = build_expr e2 var_table builder in
        L.build_select cond e1' e2' "tmp" builder
    | SListExp shrexlst ->
        let emptylist =
          L.build_call newEmptyList_f [||] "newEmptyList" builder
        in
        let rec build_list slst llst =
          match slst with
          | h :: t ->
              let e = build_expr h var_table builder in
              let node = L.build_call newNode_f [|e|] "newNode" builder in
              let llst' =
                L.build_call appendNode_f [|llst; node|] "appendNode" builder
              in
              build_list t llst'
          | [] -> llst
        in
        build_list shrexlst emptylist
    | SFunExp (formals, e) -> (
        let add_formals builder m f p =
          match f with
          | A.Formal (n, f_typ) ->
              L.set_value_name n p ;
              let var = L.define_global n (L.const_null(L.type_of p)) the_module in
              ignore(L.build_store p var builder);
              StringMap.add n (t, var) m
              (* let local = L.build_alloca (L.type_of p) n builder in
              ignore (L.build_store p local builder) ;
              StringMap.add n (f_typ, local) m *)
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
            (* let params_list = List.map (fun x -> L.dump_value x; x)
               (Array.to_list (L.params f)) in *)
            let params_list = Array.to_list (L.params f) in
            let entry_bb = L.entry_block f in
            let new_var_table =
              List.fold_left2
                (add_formals (L.builder_at_end context entry_bb))
                var_table formals params_list
            in
            ignore
              (L.build_ret
                 (build_expr e new_var_table
                    (L.builder_at_end context entry_bb) )
                 (L.builder_at_end context entry_bb) ) ;
            f
        | _ -> failwith "not a function" )
    | SFunApp (fexp, args) ->
        let f = build_expr fexp var_table builder in
        let llargs =
          List.map (fun x -> build_expr x var_table builder) args
        in
        L.build_call f (Array.of_list llargs) "result" builder
    | SAssign (id, rhs, exp) ->
        let new_var_table =
          let t = fst rhs in
          let rhs' = build_expr rhs var_table builder in
          let var = L.define_global id (L.const_null(L.type_of rhs')) the_module in
          ignore(L.build_store rhs' var builder);
          StringMap.add id (t, var) var_table
        in
        build_expr exp new_var_table builder
    | SAssignRec (id, rhs, exp) ->
      let new_var_table =
        let t = fst rhs in
        let var = L.define_global id (L.const_null(ltype_of_typ t)) the_module in
        let temp = StringMap.add id (t, var) var_table in
        let rhs' = build_expr rhs temp builder in
        ignore(L.build_store rhs' var builder);
        temp
      in
      build_expr exp new_var_table builder
    | SVar var -> (
        let v = StringMap.find var var_table in
        match v with _, llv -> L.build_load llv var builder )
    | _ -> failwith "unimplemented"
  in
  ignore
    (L.build_ret
       (build_expr program StringMap.empty builder_init)
       builder_init ) ;
  ignore (L.build_ret (L.const_int i32_t 0) builder_init) ;
  the_module
