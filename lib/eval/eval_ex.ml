
open Base
open Stdio
open Ast
open Utils

let rec eval_expr (ex: expr) (state: program_state): value =
    (* Recursive evaluation function for expressions. Converts them into a value.*)
    match ex with
      | Value(val_x) -> val_x
      | Bin_Exp(ex1, op, ex2) -> eval_bin_op ex1 op ex2 state
      | Var_Ref(name) -> eval_var name state
      | Func_App(name, expressions) -> eval_func_app name expressions state
      | ListE(expressions) -> eval_list expressions state
      (* | _ -> Exception "Not implemented yet." *)

and eval_bin_op (ex1: expr) (op: bin_op) (ex2: expr) (state: program_state) : value =
    (* Top-level evaluation function for binary operations. 
       Using implicit castings, there can only be pair of values of the same type. *)
    match Eval_bin_op.implicit_casting (eval_expr ex1 state) (eval_expr ex2 state) with
      | (IntV x1, IntV x2) -> Eval_bin_op.eval_int_op x1 op x2
      | (FloatV x1, FloatV x2) -> Eval_bin_op.eval_float_op x1 op x2
      | (StringV x1, StringV x2) -> Eval_bin_op.eval_string_op x1 op x2
      | (BoolV x1, BoolV x2) -> Eval_bin_op.eval_bool_op x1 op x2
      | (ListV x1, v2) -> Eval_bin_op.eval_list_op x1 op v2
      | (v1, ListV x2) -> Eval_bin_op.eval_list_op x2 op v1
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
                 in Hash_utils.replace_variable state name (Value(res)); res
    in match Hash_utils.get_variable state name with
      | Some(v) -> handle_variable v
      | None -> Exception (String.concat ["NameError: name '";name;"' is not defined"])

and eval_func_app (name: string) (expressions: expr list) (state: program_state) : value =
    (* Top-level evaluation function for function applications. *)
    let rec eval_arguments (arguments: expr list) : value list = 
        (* Evaluate a list of expressions into a list of values.*)
        match arguments with
          | [] -> []
          | h::r -> (eval_expr h state) :: (eval_arguments r) 
    in let handle_variable (var: expr) : value =
        (* Determine if a variable is a function and apply it.*)
        let call_function (f: func_unapp) (f_on: func_oncall) 
                          (f_off: func_offcall) (args: value list) (state: program_state) : value =
            let get_res () : value = match f with
                            | Func_Opq(f_op) -> f_op state
                            | Func_Stat(statements) -> eval_program {state with program = statements; 
                                                    local_variables = Hashtbl.create (module String)} 
            in match f_on args state with
              | Ok() -> let res = get_res () in   
                        (match f_off state with
                          | Ok() -> res
                          | Error(e) -> Exception e)
              | Error(e) -> Exception e
        in
        match var with
          | Value(Function(f, f_on, f_off)) -> call_function f f_on f_off (eval_arguments expressions) state
          | _ -> Exception (String.concat ["EvalError: Variable ";name;" can not be evalueted with applying."])
    in match Hash_utils.get_variable state name with
      | Some(v) -> handle_variable v
      | None -> Exception (String.concat ["NameError: name '";name;"' is not defined"])

and eval_list (exs: expr list) (state: program_state) : value = 
    (* Evaluate a list of expressions into a list of values. 
       Pass up Exceptions if they come up.*)
    let tmp_res = List.map exs ~f:(fun x -> eval_expr x state)
    in let reduce_if_exception (acc: (value list, string) Result.t) (x: value) : (value list, string) Result.t =
        match (acc, x) with
          | Ok(a), Exception(e) -> Error(e)
          | Ok(a), x -> Ok(a @ [x])
          | Error(e), Exception(e2) -> Error(String.concat ~sep:"\nand\n" [e; e2])
          | Error(e), _ -> Error(e)
    in match List.fold tmp_res ~init:(Ok []) ~f:reduce_if_exception with
      | Ok(x) -> ListV x
      | Error(e) -> Exception e


and eval_if (cond: expr) (then_body: statement list) (else_body: statement list)
    (prog: program_state) : value option =
    let cond_val = eval_expr cond prog in
    let is_truthy (v: value) : bool =
        match v with
          | BoolV b -> b
          | IntV n -> n <> 0
          | FloatV f -> Float.(f <> 0.0)
          | StringV s -> not (String.is_empty s)
          | ListV l -> not (List.is_empty l)
          | Ntwo -> false
          | _ -> true
    in
    let body = if is_truthy cond_val then then_body else else_body in
    let res = eval_program {prog with program = body} in
    match res with
      | Ntwo -> None
      | v -> Some v

and eval_while (cond: expr) (body: statement list)
    (prog: program_state) : unit =
    let is_truthy (v: value) : bool =
        match v with
          | BoolV b -> b
          | IntV n -> n <> 0
          | FloatV f -> Float.(f <> 0.0)
          | StringV s -> not (String.is_empty s)
          | ListV l -> not (List.is_empty l)
          | Ntwo -> false
          | _ -> true
    in
    let rec loop () =
        let cond_val = eval_expr cond prog in
        if is_truthy cond_val then begin
            let _ = eval_program {prog with program = body} in
            loop ()
        end
    in loop ()

and eval_for (var: string) (iter: expr) (body: statement list)
    (prog: program_state) : unit =
    let iter_val = eval_expr iter prog in
    match iter_val with
      | ListV items ->
        List.iter items ~f:(fun item ->
            Hash_utils.replace_variable prog var (Value item);
            let _ = eval_program {prog with program = body} in
            ()
        )
      | _ -> raise (Failure "TypeError: object is not iterable")

and eval_fundef (name: string) (params: string list) (body: statement list)
    (prog: program_state) : unit =
    let f = Func_Stat body in
    let f_on (args: value list) (state: program_state) : (unit, string) Result.t =
        let expected = List.length params in
        let actual = List.length args in
        if expected <> actual then
            Error (Printf.sprintf "TypeError: %s() takes %d positional argument(s) but %d were given"
                     name expected actual)
        else begin
            List.iter2_exn params args ~f:(fun param arg ->
                Hash_utils.add_variable state param (Value arg)
            );
            Ok ()
        end
    in
    let f_off (state: program_state) : (unit, string) Result.t =
        Hash_utils.remove_local_variabels state;
        List.iter params ~f:(fun param ->
            Hash_utils.remove_variable state param
        );
        Ok ()
    in
    Hash_utils.replace_variable prog name (Value (Function (f, f_on, f_off)))

and eval_program (prog:program_state) : value =
    (* Interpret program and return value, if such is returned, else Ntwo. *)
    let assign_var (name: string) (exp: expr) (prog: program_state) : unit =
    (* Handle variable assignment *)
        let rec remove_self_ref (exp: expr) : expr =
        (* Remove self-reference of variable being assigned. *)
            match exp with
              | Value(x) -> exp;
              | Var_Ref(x) -> (match Hash_utils.get_variable prog x with
                                  | Some(v) -> v
                                  | None -> let msg = String.concat ["NameError: name '";x;"' is not defined"]
                                            in Value(Exception(msg))
                              )
              | Bin_Exp(x1, op, x2) -> Bin_Exp(remove_self_ref x1, op, remove_self_ref x2)
              | Func_App(x, args) -> Func_App(x, List.map args ~f:remove_self_ref)
              | ListE(x) -> ListE(List.map x ~f:remove_self_ref)
        in let cleaned_exp = remove_self_ref exp
        in Hash_utils.add_local_variable prog name cleaned_exp
    in let interpret_helper (stat: statement) (prog:program_state) : value option =
        match stat with
          | Expr(exp) -> eval_expr_top exp prog; None;
          | Assign(name, exp) -> assign_var name exp prog; None;
          | Func_Def(name, f_on, f, f_off) -> Hash_utils.add_variable prog name (Value(Function(f_on, f, f_off))); None;
          | Return(exp) -> Some (eval_expr exp prog)
          | Pass -> None;
          | If(cond, then_body, else_body) -> eval_if cond then_body else_body prog
          | While(cond, body) -> eval_while cond body prog; None;
          | For(var, iter, body) -> eval_for var iter body prog; None;
          | FunDef(name, params, body) -> eval_fundef name params body prog; None;
    in match prog.program with
      | [] -> Ntwo
      | h::r -> (
          match interpret_helper h prog with
            | None -> eval_program {prog with program=r; ip=prog.ip+1}
            | Some(v) -> v
        )


and eval_expr_top ?(print_values:bool=false) (ex: expr) (state: program_state) : unit = 
    (* Top-level evaluation function. Handles Exceptions if they come up. *)
    match eval_expr ex state with
      | Exception(e) -> print_endline "";
                        print_endline ("Exception at line " ^ Int.to_string state.ip ^ ":");
                        print_endline e; 
                        raise (Failure "Program failed.")
      | x -> if print_values then value_to_output x else ()



