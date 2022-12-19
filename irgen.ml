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

(* translate : Sast.program -> Llvm.module *)
let translate program =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which we will generate code *)
  let the_module = L.create_module context "Swamp" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.float_type context
  and void_t = L.void_type context in
  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> i8_t
    (* TODO: placeholder so exhaust etc. *)
    | A.List t -> L.pointer_type (ltype_of_typ t)
    | A.Function (_, _) | Unknown -> i32_t
  in
  (* Struct Declarations *)

  (* struct Node {
   *    void *val;
   *    struct Node *next;
   * };
   *)
  let node_t : L.lltype = L.named_struct_type context "Node" in
  L.struct_set_body node_t
    [|L.pointer_type void_t; L.pointer_type node_t|]
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
    L.function_type (L.pointer_type node_t) [|L.pointer_type void_t|]
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
  let rec build_expr ((t, e) : shrexpr) (var_table : L.llvalue StringMap.t) =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    | SCharLit c -> L.const_int i8_t (Char.code c)
    | SStringLit s -> L.const_stringz context s
    | SInfixOp (e1, op, e2) ->
        let e1' = build_expr e1 var_table
        and e2' = build_expr e2 var_table in
        (* t1 == t2 bc we semanted *)
        ( match op with
        | A.Add -> (
          match e1 with
          | A.Int, _ -> L.build_add 
          | A.Float, _ -> L.build_fadd )
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
        | UMinus | Cat | Cons | Head | Tail -> L.build_add )
          e1' e2' "tmp" builder
    (* TODO: placeholders *)
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
    | SAssign (id, rhs, exp) ->
        let var = L.build_alloca (ltype_of_typ t) id builder in
        let temp = StringMap.add id var var_table in
        let rhs' = build_expr rhs var_table in
        ignore (L.build_store rhs' var builder) ;
        build_expr exp temp
    | SVar var -> L.build_load (StringMap.find var var_table) var builder
    | SListExp shrexlst ->
        let emptylist =
          L.build_call newEmptyList_f [||] "newEmptyList" builder
        in
        let rec build_list slst llst =
          match slst with
          | h :: t ->
              let e = build_expr h var_table in
              let node = L.build_call newNode_f [|e|] "newNode" builder in
              let llst' =
                L.build_call appendNode_f [|llst; node|] "appendNode" builder
              in
              build_list t llst'
          | [] -> llst
        in
        build_list shrexlst emptylist
     | SAssignRec (_, _, _)
     | SParenExp _
     |SListComp (_, _)
     |SFunExp (_, _)
     |SFunApp (_, _) ->
        L.const_int i32_t 0
  in
  ignore (L.build_ret (build_expr program StringMap.empty) builder) ;
  the_module
