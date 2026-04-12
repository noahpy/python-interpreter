
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
      | AccessE(ex1, ex2) -> eval_access ex1 ex2 state
      | DictE(pairs) -> eval_dict pairs state
      | SliceE(_, _, _) -> Exception "SyntaxError: slice expression outside of subscript"
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
      | (DictV x1, v2) -> Eval_bin_op.eval_dict_op (DictV x1) op v2
      | (v1, DictV x2) -> Eval_bin_op.eval_dict_op v1 op (DictV x2)
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
            let get_res () : (value * program_state) = match f with
                            | Func_Opq(f_op) -> (f_op state, state)
                            | Func_Stat(statements) -> 
                                    let new_state = {state with program = statements; 
                                                     local_variables = Hashtbl.create (module String)}
                                    in (eval_program new_state, new_state)
            in match f_on args state with
              | Ok() -> (
                  let res, new_state = get_res () in
                  match f_off new_state with
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


and eval_access (accessed: expr) (key: expr) (state: program_state) : value =
    let accessed_val = eval_expr accessed state in
    match key with
      | SliceE(s, e, st) -> eval_slice accessed_val s e st state
      | _ ->
    let key_val = eval_expr key state in
    match accessed_val with
      | ListV l -> (
          match key_val with
            | IntV i -> let length = List.length l in
                        let index = if i < 0 then length+i else i in
                        (match List.nth l index with
                          | Some(x) -> x
                          | None -> Exception("IndexError: list index out of range")
                        )
            | _ -> Exception ("TypeError: list indices must be integers, not " ^ Sexp.to_string (sexp_of_value key_val))
            )
      | DictV h -> (
          let s = value_to_str ~add_paren:true key_val in
          match Hashtbl.find h s with
                            | Some(x) -> x
                            | None -> Exception("KeyError: '" ^ s ^ "' not in dictionary")
                          )
      | StringV s -> (
          match key_val with
            | IntV i -> let length = String.length s in
                        let index = if i < 0 then length+i else i in
                        (match List.nth (String.to_list s) index with
                          | Some(x) -> StringV (String.of_char x)
                          | None -> Exception("IndexError: string index out of range")
                        )
            | _ -> Exception ("TypeError: string indices must be integers, not " ^ Sexp.to_string (sexp_of_value key_val))
      )
      | x -> Exception (String.concat ["TypeError: ";Sexp.to_string (sexp_of_value x);" object is not subscriptable."])

and eval_slice (accessed_val: value) (start: expr option) (stop: expr option)
               (step: expr option) (state: program_state) : value =
    (* Evaluate an optional slice component to (int option, error). *)
    let eval_opt (e: expr option) : (int option, string) Result.t =
        match e with
          | None -> Ok None
          | Some(ex) -> (match eval_expr ex state with
                          | IntV i -> Ok (Some i)
                          | Exception s -> Error s
                          | v -> Error ("TypeError: slice indices must be integers or None, not "
                                        ^ Sexp.to_string (sexp_of_value v)))
    in
    (* Compute Python-style (start, stop, step) clamped indices for a sequence of given length. *)
    let compute_indices (length: int) : (int * int * int, string) Result.t =
        match eval_opt start, eval_opt stop, eval_opt step with
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
          | Ok s, Ok e, Ok st ->
              let step_v = Option.value st ~default:1 in
              if step_v = 0 then Error "ValueError: slice step cannot be zero"
              else
                  let lower, upper =
                      if step_v > 0 then 0, length else -1, length - 1
                  in
                  let normalize x =
                      let x = if x < 0 then x + length else x in
                      if x < lower then lower
                      else if x > upper then upper
                      else x
                  in
                  let start_v = match s with
                    | None -> if step_v < 0 then upper else lower
                    | Some v -> normalize v
                  in
                  let stop_v = match e with
                    | None -> if step_v < 0 then lower else upper
                    | Some v -> normalize v
                  in
                  Ok (start_v, stop_v, step_v)
    in
    let do_slice (type a) (items: a array) (length: int) : (a list, string) Result.t =
        match compute_indices length with
          | Error e -> Error e
          | Ok (start_v, stop_v, step_v) ->
              let rec gen i acc =
                  if (step_v > 0 && i >= stop_v) || (step_v < 0 && i <= stop_v) then
                      List.rev acc
                  else if i < 0 || i >= length then List.rev acc
                  else gen (i + step_v) (items.(i) :: acc)
              in Ok (gen start_v [])
    in
    match accessed_val with
      | ListV l -> (
          let arr = Array.of_list l in
          match do_slice arr (Array.length arr) with
            | Ok xs -> ListV xs
            | Error e -> Exception e
          )
      | StringV s -> (
          let arr = String.to_array s in
          match do_slice arr (Array.length arr) with
            | Ok xs -> StringV (String.of_char_list xs)
            | Error e -> Exception e
          )
      | Exception e -> Exception e
      | x -> Exception (String.concat ["TypeError: ";Sexp.to_string (sexp_of_value x);" object is not subscriptable."])


and eval_dict (pairs: (expr * expr) list) (state: program_state) : value =
    let tmp_res = List.map pairs ~f:(fun (k, v) -> (value_to_str ~add_paren:true (eval_expr k state), eval_expr v state))
    in match Hashtbl.of_alist_report_all_dups (module String) tmp_res with
      | `Ok(h) -> DictV h
      | `Duplicate_keys keys -> print_endline "Warning: Duplicate keys in dictionary literal."; 
                                let filtered_pairs = List.filter tmp_res ~f:(fun (k, v) -> not (List.mem keys k ~equal:String.equal)) in
                                DictV (Hashtbl.of_alist_exn (module String) filtered_pairs)


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
      | StringV s ->
        String.iter s ~f:(fun c ->
            Hash_utils.replace_variable prog var (Value (StringV (String.of_char c)));
            let _ = eval_program {prog with program = body} in
            ()
        )
      | DictV h ->
        let keys = Hashtbl.keys h in
        List.iter keys ~f:(fun k ->
            Hash_utils.replace_variable prog var (Value (StringV k));
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


and eval_assign_var (name: string) (exp: expr) (index: expr option) (prog: program_state) : unit =
    (* Handle variable assignment *)
    let handle_index_assign (name: string) (exp: expr) (index: expr) (prog: program_state) : unit =
        let index_val = eval_expr_exp index prog in
        let accessed_val = eval_expr_exp (Var_Ref(name)) prog in
        match accessed_val with
          | ListV l -> (
              match index_val with
                | IntV i -> 
                    let length = List.length l in
                    let adjusted_i = if i < 0 then i + length else i in
                    if adjusted_i < 0 || adjusted_i >= length then 
                        raise (Failure ("IndexError: list " ^ name ^ "index out of range with: " ^ Int.to_string i))
                    else
                        let new_val = eval_expr_exp exp prog in
                        let new_list = List.mapi l ~f:(fun j v -> if j = adjusted_i then new_val else v) in
                        Hash_utils.replace_variable prog name (Value(ListV new_list))
                | _ -> raise (Failure "TypeError: indices must be integers")
              )
          | DictV h -> (
              let s = value_to_str ~add_paren:true index_val in
              let new_val = eval_expr_exp exp prog in
              Hashtbl.set h ~key:s ~data:new_val;
            )
          | _ -> raise (Failure ("TypeError: object " ^ name ^ " does not support item assigmnent."))
    in
    let rec remove_ref (exp: expr) : expr =
    (* Remove reference of variables, including self-reference. *)
        match exp with
          | Value(x) -> exp;
          | Var_Ref(x) -> (match Hash_utils.get_variable prog x with
                              | Some(v) -> v
                              | None -> let msg = String.concat ["NameError: name '";x;"' is not defined"]
                                        in Value(Exception(msg))
                          )
          | Bin_Exp(x1, op, x2) -> Bin_Exp(remove_ref x1, op, remove_ref x2)
          | Func_App(x, args) -> Func_App(x, List.map args ~f:remove_ref)
          | ListE(x) -> ListE(List.map x ~f:remove_ref)
          | AccessE(x1, x2) -> AccessE(remove_ref x1, remove_ref x2)
          | DictE(x) -> DictE(List.map x ~f:(fun (k, v) -> (remove_ref k, remove_ref v)))
          | SliceE(s, e, st) -> SliceE(Option.map s ~f:remove_ref,
                                       Option.map e ~f:remove_ref,
                                       Option.map st ~f:remove_ref)
    in match index with
      | Some(i) -> handle_index_assign name (remove_ref exp) i prog
      | None -> let cleaned_exp = remove_ref exp
                in Hash_utils.add_local_variable prog name cleaned_exp

and eval_program (prog:program_state) : value =
    (* Interpret program and return value, if such is returned, else Ntwo. *)
    let interpret_helper (stat: statement) (prog:program_state) : value option =
        match stat with
          | Expr(exp) -> eval_expr_top exp prog; None;
          | Assign(name, exp, index) -> eval_assign_var name exp index prog; None;
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

and eval_expr_exp (ex: expr) (state: program_state) : value =
    match eval_expr ex state with
      | Exception(e) -> print_endline "";
                        print_endline ("Exception at statement " ^ Int.to_string state.ip ^ ":");
                        print_endline e
                      ; raise (Failure "Program failed.")
      | x -> x

and eval_expr_top ?(print_values:bool=false) (ex: expr) (state: program_state) : unit = 
    (* Top-level evaluation function. Handles Exceptions if they come up. *)
    let x = eval_expr_exp ex state in
    if print_values then value_to_output x else ()



