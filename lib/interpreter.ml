
open Ast
open Eval

let init_program_state (lines: statement list) (var_size: int): program_state =
    let varH = Hashtbl.create var_size in 
    let p = {program = lines; ip = 0; variables = varH;} in
    Stdlib.Load.load_impls p; p


let rec interpret (prog:program_state) : unit = 
    let assign_var (name: string) (exp: expr) (prog: program_state) : unit =
    (* Handle variable assignment *)
        let rec remove_self_ref (exp: expr) : expr =
        (* Remove self-reference of variable being assigned. *)
            match exp with
              | Value(x) -> exp;
              | Var_Ref(x) -> if x = name then
                                match Hashtbl.find_opt prog.variables x with
                                  | Some(v) -> v
                                  | None -> let msg = String.concat "" ["NameError: name '";x;"' is not defined"]
                                            in Value(Exception(msg))
                              else exp
              | Bin_Exp(x1, op, x2) -> Bin_Exp(remove_self_ref x1, op, remove_self_ref x2)
              | Func_App(x, args) -> Func_App(x, List.map remove_self_ref args)
        in let cleaned_exp = remove_self_ref exp
        in Hashtbl.add prog.variables name cleaned_exp
    in let interpret_helper (stat: statement) (prog:program_state) : unit = 
        match stat with
          | Expr(exp) -> Eval_ex.eval_expr_top exp prog
          | Assign(name, exp) -> assign_var name exp prog
          | Func_Def(name, exp) -> print_endline "Function definition not implemented!"
    in match prog.program with
      | [] -> ()
      | h::r -> interpret_helper h prog; interpret {prog with program=r; ip=prog.ip+1}
