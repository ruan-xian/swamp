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
  and i1_t = L.i1_type context in
  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    (* TODO: placeholder so exhaust etc. *)
    | A.Float | A.Char | A.String | A.List _ | A.Function (_, _) -> i32_t
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
  let rec build_expr ((_, e) : shrexpr) =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SInfixOp (e1, op, e2) ->
        let e1' = build_expr e1 and e2' = build_expr e2 in
        ( match op with
        | A.Add -> L.build_add
        | A.Sub -> L.build_sub
        (* TODO: PLACEHOLDERS *)
        | Mul | Div | Mod | Eq | Less | Greater | Geq | Leq | Neq | And
         |Or | Not | UMinus | Cat | Cons | Head | Tail ->
            L.build_add )
          e1' e2' "tmp" builder
    (* TODO: placeholders *)
    | SUnaryOp (_, _)
     |SCondExp (_, _, _)
     |SAssign (_, _, _)
     |SAssignRec (_, _, _)
     |SVar _ | SFloatLit _ | SStringLit _ | SCharLit _ | SParenExp _
     |SListExp _
     |SListComp (_, _)
     |SFunExp (_, _)
     |SFunApp (_, _) ->
        L.const_int i32_t 0
  in
  ignore (L.build_ret (build_expr program) builder) ;
  the_module
