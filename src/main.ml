
open Ast

let create_execution_engine () =
  ignore (Llvm_executionengine.initialize_native_target ());
  Llvm_executionengine.ExecutionEngine.create Codegen.the_module

let create_pass_manager the_execution_engine =
  (* Create the JIT. *)
  let the_pm = Llvm.PassManager.create () in

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add (Llvm_executionengine.ExecutionEngine.target_data the_execution_engine) the_pm;

  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_pm;

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  Llvm_scalar_opts.add_instruction_combination the_pm;

  (* reassociate expressions. *)
  Llvm_scalar_opts.add_reassociation the_pm;

  (* Eliminate Common SubExpressions. *)
  Llvm_scalar_opts.add_gvn the_pm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  Llvm_scalar_opts.add_cfg_simplification the_pm;

  the_pm


let main ch =
  (* initialization of LLVM *)
  let the_execution_engine = create_execution_engine () in
  let the_pm = create_pass_manager the_execution_engine in
  (* parsing *)
  let ast = Parser.parse_channel Parser.program ch in
    (* NOTE: replace Parser.expression by Parser.statement then by Parser.program according to your progression. *)
  (* code generation *)
  ignore (Codegen.gen_program ast);
  Llvm.dump_module Codegen.the_module;
  (* optimization *)
  let _ = Llvm.PassManager.run_module Codegen.the_module the_pm in
  if false then
    Llvm.dump_module Codegen.the_module
  else ()

let _ =
  let ch =
    try open_in Sys.argv.(1)
    with _ -> stdin in
  main ch
