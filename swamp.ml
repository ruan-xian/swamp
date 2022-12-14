type action = Ast | Sast (*| LLVM_IR*)

let () =
  let action = ref Ast in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    (* ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR"); *)
  ] in
  let usage_msg = "usage: ./swamp.native [-a|-s|-l] [file.swamp]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in

  let ast = Parser.program Scanner.tokenize lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_prog ast)
  | _ -> let sast = Semant.check ast in
    match !action with
      Ast     -> ()
    | Sast    -> print_string ((Sast.string_of_shrexpr sast) ^ "\n")
    (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast)) *)
