
open Ast


let rec eval_expr (ex: expr) (state: program_state): value =
    (* Recursive evaluation function for expressions. Converts them into a value.*)
    match ex with
      | Value(val_x) -> val_x
      | Bin_Exp(ex1, op, ex2) -> eval_bin_op ex1 op ex2 state
      | Var_Ref(name) -> eval_var name state
      | Func_App(name, expressions) -> eval_func_app name expressions state
      (* | _ -> Exception "Not implemented yet." *)

and eval_bin_op (ex1: expr) (op: bin_op) (ex2: expr) (state: program_state) : value =
    (* Top-level evaluation function for binary operations. 
       Using implicit castings, there can only be pair of values of the same type. *)
    match Eval_bin_op.implicit_casting (eval_expr ex1 state) (eval_expr ex2 state) with
      | (IntV x1, IntV x2) -> Eval_bin_op.eval_int_op x1 op x2
      | (FloatV x1, FloatV x2) -> Eval_bin_op.eval_float_op x1 op x2
      | (StringV x1, StringV x2) -> Eval_bin_op.eval_string_op x1 op x2
      | (BoolV x1, BoolV x2) -> Eval_bin_op.eval_bool_op x1 op x2
      | (Exception e1, Exception e2) -> Exception (e1 ^ "\nand\n" ^ e2)
      | (Exception e1, _) -> Exception(e1)
      | (_, Exception e2) -> Exception(e2)
      | _ -> Exception "Can not operate on these types: Implicit type conversion missing."

and eval_var (name: string) (state: program_state) : value = 
    (* Top-level evaluation function for variables. *)
    let handle_variable (var: expr) : value = 
        (* Determine if a variable is a value and return it, if not evaluate, save and return it.*)
        match var with 
          | Value(v) -> v
          | _ -> let res = eval_expr var state 
                 in Hashtbl.replace state.variables name (Value(res)); res
    in match Hashtbl.find_opt state.variables name with
      | Some(v) -> handle_variable v
      | None -> Exception (String.concat "" ["NameError: name '";name;"' is not defined"])

and eval_func_app (name: string) (expressions: expr list) (state: program_state) : value =
    (* Top-level evaluation function for function applications. *)
    let rec eval_arguments (arguments: expr list) : value list = 
        (* Evaluate a list of expressions into a list of values.*)
        match arguments with
          | [] -> []
          | h::r -> (eval_expr h state) :: (eval_arguments r) 
    in let handle_variable (var: expr) : value =
        (* Determine if a variable is a function and apply it.*)
        match var with
          | Value(Function(f)) -> f (eval_arguments expressions) state
          | _ -> Exception (String.concat "" ["EvalError: Variable ";name;" can not be evalueted with applying."])
    in match Hashtbl.find_opt state.variables name with
      | Some(v) -> handle_variable v
      | None -> Exception (String.concat "" ["NameError: name '";name;"' is not defined"])


let eval_expr_top ?(print_values:bool=false) (ex: expr) (state: program_state) : unit = 
    (* Top-level evaluation function. Handles Exceptions if they come up. *)
    match eval_expr ex state with
      | Exception(e) -> print_newline();
                        print_endline ("Exception at line " ^ string_of_int state.ip ^ ":");
                        print_endline e; 
                        raise (Failure "Program failed.")
      | x -> if print_values then value_to_output x else ()

