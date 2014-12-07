
open Ast

exception TODO (* to be used for actions remaining to be done *)
exception Error of string (* to be used for semantic errors *)

(* global context, main module, and builder for generating code *)

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "main"
let builder = Llvm.builder context

(* LLVM types for VSL+ *)

let int_type = Llvm.i32_type context
let void_type = Llvm.void_type context
let char_type = Llvm.i8_type context
let string_type = Llvm.pointer_type char_type
let int_array_type = Llvm.array_type int_type 0

(* generation of constant integer LLVM values *)

let const_int n = Llvm.const_int int_type n

let zero_int = const_int 0

(* generation of constant string LLVM values *)

let const_string =
  let string_gep_indices = [|zero_int; zero_int|] in
  fun s ->
    let const_s = Llvm.const_stringz context s in
    let global_s = Llvm.define_global s const_s the_module in
    Llvm.const_gep global_s string_gep_indices

(* the printf function as a LLVM value *)

let func_printf =
  let tf = Llvm.var_arg_function_type int_type [|string_type|] in
  let f = Llvm.declare_function "printf" tf the_module in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.add_param_attr (Llvm.param f 0) Llvm.Attribute.Nocapture;
  f

(* the scanf function as a LLVM value *)

let func_scanf =
  let tf = Llvm.var_arg_function_type int_type [|string_type|] in
  let f = Llvm.declare_function "scanf" tf the_module in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.add_param_attr (Llvm.param f 0) Llvm.Attribute.Nocapture;
  f

(* Create an alloca instruction in the entry block of the
function. This is used for mutable local variables. *)

let create_entry_block_alloca the_function var_name typ =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  Llvm.build_alloca typ var_name builder

let create_entry_block_array_alloca the_function var_name typ size =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  let vsize = Llvm.const_int int_type size in
  Llvm.build_array_alloca typ vsize var_name builder

(* generation of code for each VSL+ construct *)

let rec gen_expression : expression -> Llvm.llvalue = function
  | Const n ->
    (* returns a constant llvalue for that integer *)
    const_int n
  | Plus (e1,e2) ->
    (* generates the code for [e1] and returns the result llvalue *)
    let t1 = gen_expression e1 in
    (* the same for e2 *)
    let t2 = gen_expression e2 in
    (* appends an 'add' instruction and returns the result llvalue *)
    Llvm.build_add t1 t2 "add_tmp" builder
  (* TODO NAT? *)
  | Minus (e1, e2) ->
    let t1 = gen_expression e1 in
    let t2 = gen_expression e2 in
    Llvm.build_sub t1 t2 "sub_tmp" builder
  | Mul (e1, e2) ->
    let t1 = gen_expression e1 in
    let t2 = gen_expression e2 in
    Llvm.build_mul t1 t2 "mul_tmp" builder
  | Div (e1, e2) ->
    let t1 = gen_expression e1 in
    let t2 = gen_expression e2 in
    Llvm.build_sdiv t1 t2 "sdiv_tmp" builder
  | Expr_Ident id -> SymbolTableList.lookup id
  | ArrayElem (id, e1) -> failwith "TODO"
    (* let t1 = gen_expression e1 in
    Llvm.build_extractvalue (SymbolTableList.lookup id) t1 "extractvalue_tmp" builder *)
  | ECall (_callee, _args) ->
    let callee = SymbolTableList.lookup _callee in
    let args = Array.map gen_expression _args in
    Llvm.build_call callee args "call_tmp" builder

let rec gen_statement : statement -> Llvm.llvalue = function
  | Assign (lhs, expr) -> failwith "TODO"
  | Return expr -> failwith "TODO"
  | SCall (callee, args) -> failwith "TODO"
  | Print items -> failwith "TODO"
  | Read items -> failwith "TODO"
  | Block (declarations, statements) -> failwith "TODO"
  | If (_condition, _then_statement, _else_statement) ->
    (* condition *)
    let condition = gen_expression _condition in
    let condition_bool = Llvm.build_icmp Llvm.Icmp.Ne condition zero_int "icmp_tmp" builder in
    (* blocks *)
    let start_bb = Llvm.insertion_block builder in
    let parent_bb = Llvm.block_parent start_bb in
    (* then *)
    let then_bb = Llvm.append_block context "then" parent_bb in
    Llvm.position_at_end then_bb builder;
    ignore (gen_statement _then_statement);
    assert (then_bb = Llvm.insertion_block builder);
    let else_bb_option =
      match _else_statement with
      | Some _else_statement' ->
        begin
          (* else *)
          let else_bb = Llvm.append_block context "else" parent_bb in
          Llvm.position_at_end else_bb builder;
          ignore(gen_statement _else_statement');
          assert (else_bb = Llvm.insertion_block builder);
          Some else_bb
        end
      | None -> None
    in
    (* merge *)
    let merge_bb = Llvm.append_block context "ifcont" parent_bb in
    (* start -> then | else *)
    Llvm.position_at_end start_bb builder;
    begin
      match else_bb_option with
        | Some else_bb -> ignore (Llvm.build_cond_br condition_bool then_bb else_bb builder)
        | None -> ignore (Llvm.build_cond_br condition_bool then_bb merge_bb builder)
    end;
    (* then -> merge *)
    Llvm.position_at_end then_bb builder;
    ignore (Llvm.build_br merge_bb builder);
    (* else -> merge *)
    begin
      match else_bb_option with
        | Some else_bb ->
          Llvm.position_at_end else_bb builder;
          ignore (Llvm.build_br merge_bb builder)
        | None -> ()
    end;
    (* position *)
    Llvm.position_at_end merge_bb builder;
    Llvm.value_of_block merge_bb (* TODO ? *)
  | While (condition, statement) -> failwith "TODO"

(* function that turns the code generated for an expression into a valid LLVM code *)
let gen (e : expression) : unit =
  let the_function = Llvm.declare_function "main" (Llvm.function_type int_type [||]) the_module in
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;
  let x = gen_expression e in
  ignore (Llvm.build_ret x builder)
