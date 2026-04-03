
open Base
open Stdio
open Ast
open Eval
open Utils

let init_program_state ?(var_size: int=1) ?(load_stdlib: bool=false) (lines: statement list) : program_state =
    let varH = Hashtbl.create ~size:var_size (module String) in 
    let p = {program = lines; ip = 0; variables = varH;} in
    if load_stdlib then (Python_stdlib.Load.load_impls p; p) else p


let rec interpret (prog:program_state) : unit = 
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
        in let cleaned_exp = remove_self_ref exp
        in Hash_utils.replace_variable prog name cleaned_exp
    in let interpret_helper (stat: statement) (prog:program_state) : unit = 
        match stat with
          | Expr(exp) -> Eval_ex.eval_expr_top exp prog
          | Assign(name, exp) -> assign_var name exp prog
          | Func_Def(name, f_on, f, f_off) -> Hash_utils.add_variable prog name (Value(Function(f_on, f, f_off)))
    in match prog.program with
      | [] -> ()
      | h::r -> interpret_helper h prog; interpret {prog with program=r; ip=prog.ip+1}
