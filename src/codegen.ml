
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

let create_entry_block_alloca_2 the_function var_name typ =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  Llvm.build_alloca typ var_name builder

let create_entry_block_array_alloca_2 the_function var_name typ size =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  let vsize = Llvm.const_int int_type size in
  Llvm.build_array_alloca typ vsize var_name builder

(* generation of code for each VSL+ construct *)

let current_function_builder = ref None

let rec gen_expression : expression -> Llvm.llvalue = function
  | Const n -> const_int n
  | Plus (e1,e2) ->
    let t1 = gen_expression e1 in
    let t2 = gen_expression e2 in
    Llvm.build_add t1 t2 "add_tmp" builder
  | Minus (e1, e2) -> (* TODO NAT? *)
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
  | Expr_Ident id ->
    let value = SymbolTableList.lookup id in (* TODO error message *)
    let typ = Llvm.type_of value in
    if typ = int_type then
      value
    else if typ = Llvm.pointer_type int_type then
      Llvm.build_load value id builder
    else
      raise (Error (id ^ " is not an int!"))
  | ArrayElem (id, expr) ->
    let array = SymbolTableList.lookup id in (* TODO error message *)
    let index = gen_expression expr in
    let value = Llvm.build_gep array [|index|] id builder in
    Llvm.build_load value id builder
  | ECall (_callee, _args) ->
    let callee = SymbolTableList.lookup _callee in (* TODO error message *)
    let return_type = Llvm.return_type (Llvm.element_type (Llvm.type_of callee)) in
    if return_type <> int_type then
      raise (Error ("The function " ^ _callee
      ^ " appears in an expression and therefore should have " ^ (Llvm.string_of_lltype int_type)
      ^ " as return type but its return type it " ^ (Llvm.string_of_lltype return_type) ^ "!"))
    else
      let args = Array.map gen_expression _args in
      Llvm.build_call callee args "call_tmp" builder

let value_of_lhs = function (* TODO types *)
  | LHS_Ident id ->
    let value = SymbolTableList.lookup id in (* TODO error message *)
    let typ = Llvm.type_of value in
    if typ = Llvm.pointer_type int_type then
      value
    else
      raise (Error (id ^ " is not an int!"))
  | LHS_ArrayElem (id, expr) ->
    let array = SymbolTableList.lookup id in (* TODO error message *)
    if Llvm.type_of array <> Llvm.pointer_type int_type then (* Checking for pointer instead of array because LLVM doesn't seem to remember it's actually an array *)
      raise (Error (id ^ " has type " ^ (Llvm.string_of_lltype (Llvm.type_of array))
      ^ " but was expected to be of type " ^ (Llvm.string_of_lltype int_array_type)
      ^ " = " ^ (Llvm.string_of_lltype (Llvm.pointer_type int_type)) ^ "!"))
    else
      let index = gen_expression expr in
      Llvm.build_gep array [|index|] id builder

let rec gen_statement : statement -> unit = function
  | Assign (lhs, expr) ->
    let value = gen_expression expr in
    let storage = value_of_lhs lhs in
    ignore (Llvm.build_store value storage builder)
  | Return expr -> ignore(Llvm.build_ret (gen_expression expr) builder)
  | SCall (_callee, _args) ->
    let callee = SymbolTableList.lookup _callee in (* TODO error message *)
    let return_type = Llvm.return_type (Llvm.element_type (Llvm.type_of callee)) in
    if return_type <> void_type then
      raise (Error ("The function " ^ _callee
      ^ " appears as a statement and therefore should have " ^ (Llvm.string_of_lltype void_type)
      ^ " as return type but its return type it " ^ (Llvm.string_of_lltype return_type) ^ "!"))
    else
      let args = Array.map gen_expression _args in
      ignore (Llvm.build_call callee args "" builder)
  | Print items ->
    let formats = List.map (function
      | Print_Expr _ -> "%d"
      | Print_Text _ -> "%s"
    ) items in
    let format = String.concat "" formats in
    let format_value = const_string format in
    let items_values = List.map (function
      | Print_Expr e -> gen_expression e
      | Print_Text s -> const_string s
    ) items in
    let args = Array.of_list (format_value :: items_values) in
    ignore (Llvm.build_call func_printf args "printf" builder)
  | Read items ->
      let formats = List.map (fun _ -> "%d") items in
      let format = String.concat "" formats in
      let format_value = const_string format in
      let items_values = List.map value_of_lhs items in
      let args = Array.of_list (format_value :: items_values) in
      ignore (Llvm.build_call func_scanf args "scanf" builder)
  | Block (declarations, statements) ->
    SymbolTableList.open_scope ();
    List.iter (fun declaration ->
      let id, value =
        let new_builder =
          match !current_function_builder with
          | None -> failwith "This should not happen... Trying to generate a block statement outside of a function..."
          | Some b -> b
        in

        match declaration with
          | Dec_Ident id -> id, Llvm.build_alloca int_type id new_builder
          | Dec_Array (id, n) -> id, Llvm.build_array_alloca int_type (const_int n) id new_builder
      in
      SymbolTableList.add id value
    ) declarations;
    List.iter gen_statement statements;
    SymbolTableList.close_scope ();
  | If (_condition, _then_statement, _else_statement) ->
    (* condition *)
    let condition = gen_expression _condition in
    let condition_bool = Llvm.build_icmp Llvm.Icmp.Ne condition zero_int "icmp_tmp" builder in
    (* blocks *)
    let start_bb = Llvm.insertion_block builder in
    let parent_bb = Llvm.block_parent start_bb in
    (* then *)
    let then_bb = Llvm.append_block context "then_bb" parent_bb in
    Llvm.position_at_end then_bb builder;
    ignore (gen_statement _then_statement);
    let then_bb_2 = Llvm.insertion_block builder in
    let else_bb_1_and_2_option =
      match _else_statement with
      | Some _else_statement' ->
        begin
          (* else *)
          let else_bb = Llvm.append_block context "else_bb" parent_bb in
          Llvm.position_at_end else_bb builder;
          ignore(gen_statement _else_statement');
          let else_bb_2 = Llvm.insertion_block builder in
          Some (else_bb, else_bb_2)
        end
      | None -> None
    in
    (* merge *)
    let merge_bb = Llvm.append_block context "merge_bb" parent_bb in
    (* start -> then | else *)
    Llvm.position_at_end start_bb builder;
    begin
      match else_bb_1_and_2_option with
        | Some (else_bb, _) -> ignore (Llvm.build_cond_br condition_bool then_bb else_bb builder)
        | None -> ignore (Llvm.build_cond_br condition_bool then_bb merge_bb builder)
    end;
    (* then -> merge *)
    Llvm.position_at_end then_bb_2 builder;
    ignore (Llvm.build_br merge_bb builder);
    (* else -> merge *)
    begin
      match else_bb_1_and_2_option with
        | Some (_, else_bb_2) ->
          Llvm.position_at_end else_bb_2 builder;
          ignore (Llvm.build_br merge_bb builder)
        | None -> ()
    end;
    (* position *)
    Llvm.position_at_end merge_bb builder
  | While (_condition, statement) ->
    (* blocks *)
    let start_bb = Llvm.insertion_block builder in
    let parent_bb = Llvm.block_parent start_bb in
    (* test *)
    let test_bb = Llvm.append_block context "test_bb" parent_bb in
    (* body *)
    let body_bb = Llvm.append_block context "body_bb" parent_bb in
    Llvm.position_at_end body_bb builder;
    ignore (gen_statement statement);
    let body_bb_2 = Llvm.insertion_block builder in
    (* out *)
    let out_bb = Llvm.append_block context "out_bb" parent_bb in
    (* start -> test *)
    Llvm.position_at_end start_bb builder;
    ignore (Llvm.build_br test_bb builder);
    (* test -> body *)
    Llvm.position_at_end test_bb builder;
    let condition = gen_expression _condition in
    let condition_bool = Llvm.build_icmp Llvm.Icmp.Ne condition zero_int "icmp_tmp" builder in
    ignore (Llvm.build_cond_br condition_bool body_bb out_bb builder);
    (* body -> test *)
    Llvm.position_at_end body_bb_2 builder;
    ignore (Llvm.build_br test_bb builder);
    (* position *)
    Llvm.position_at_end out_bb builder

let gen_program_unit program_unit =
  (* get proto *)
  let (typ, id, params) =
    match program_unit with
    | Proto proto
    | Function (proto, _) -> proto
  in
  (* build function type *)
  let return_type =
    match typ with
    | Type_Int -> int_type
    | Type_Void -> void_type
  in
  let params_types = Array.map (fun _ -> int_type) params in
  let function_type = Llvm.function_type return_type params_types in
  (* if already defined, check that types are equal *)
  let function_value =
    try
      let value = SymbolTableList.lookup id in
      if function_type <> Llvm.element_type (Llvm.type_of value) then
        let previous_function_type_string = Llvm.string_of_lltype (Llvm.element_type (Llvm.type_of value)) in
        let new_function_type_string = Llvm.string_of_lltype function_type in
        raise (Error ("The function " ^ id ^ " has previously been declared with type "
        ^ previous_function_type_string ^ " and is being redeclared with incompatible type "
        ^ new_function_type_string ^ "!"))
      else if Array.length (Llvm.basic_blocks value) <> 0 then
        raise (Error ("The function " ^ id ^ " is already defined!"))
      else
        value
    with
    | Failure _ ->
      let value = Llvm.declare_function id function_type the_module in
      Llvm.add_function_attr value Llvm.Attribute.Nounwind;
      SymbolTableList.add id value;
      value
  in
  (* declare the function *)
  match program_unit with
  | Proto _ -> ()
  | Function (_, statement) ->
    let function_bb = Llvm.append_block context "function_bb" function_value in
    Llvm.position_at_end function_bb builder;
    SymbolTableList.open_scope();
    let params_values = Llvm.params function_value in
    for i = 0 to (Array.length params_values) - 1 do
      SymbolTableList.add params.(i) params_values.(i)
    done;
    current_function_builder := Some (Llvm.builder_at context (Llvm.instr_begin function_bb));
    gen_statement statement;
    SymbolTableList.close_scope();
    match typ with
    | Type_Void -> ignore(Llvm.build_ret_void builder)
    | _ -> ()

let gen_program program =
  SymbolTableList.open_scope();
  List.iter gen_program_unit program;
  SymbolTableList.close_scope()

(* function that turns the code generated for an expression into a valid LLVM code *)
let gen (e : expression) : unit =
  let the_function = Llvm.declare_function "main" (Llvm.function_type int_type [||]) the_module in
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;
  let x = gen_expression e in
  ignore (Llvm.build_ret x builder)
