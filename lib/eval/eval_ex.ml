
open Ast
open Program_state

let rec eval_expr (ex: expr) (state: program_state): value =
    (* Top-level evaluation function for expressions. Converts them into a value.*)
    match ex with
      | Value(val_x) -> val_x
      | Bin_Exp(ex1, op, ex2) -> eval_bin_op ex1 op ex2 state
      | Variable(name) -> eval_var name state
      | Func_App(name, expressions) -> eval_func_app name expressions state
      (* | _ -> Exception "Not implemented yet." *)

and eval_bin_op (ex1: expr) (op: bin_op) (ex2: expr) (state: program_state) : value =
    match Eval_bin_op.implicit_casting (eval_expr ex1 state) (eval_expr ex2 state) with
      | (IntV x1, IntV x2) -> Eval_bin_op.eval_int_op x1 op x2
      | (FloatV x1, FloatV x2) -> Eval_bin_op.eval_float_op x1 op x2
      | (StringV x1, StringV x2) -> Eval_bin_op.eval_string_op x1 op x2
      | (BoolV x1, BoolV x2) -> Eval_bin_op.eval_bool_op x1 op x2
      | (Exception e1, Exception e2) -> Exception (e1)
      | _ -> Exception "Can not operate on these types: Implicit type conversion missing."

and eval_var (name: string) (state: program_state) : value = 
    match Hashtbl.find_opt state.variables name with
      | Some(exp) -> let res = eval_expr exp state in Hashtbl.replace state.variables name (Value(res)); res
      | None -> Exception (String.concat "" ["NameError: name '";name;"' is not defined"])

and eval_func_app (name: string) (expressions: expr list) (state: program_state) : value =
    let rec eval_arguments (arguments: expr list) : value list = 
        match arguments with
          | [] -> []
          | h::r -> (eval_expr h state) :: (eval_arguments r) 
    in 
    match Hashtbl.find_opt state.functions name with
      | Some(f) -> f (eval_arguments expressions) state
      | None -> Exception (String.concat "" ["NameError: name '";name;"' is not defined"])
