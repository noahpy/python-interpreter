
open Ast
open Program_state
open Eval

let rec interpret (prog:program_state) : unit = 
    let interpret_helper (stat: statement) (prog:program_state) : unit = 
        match stat with
          | Expr(exp) -> Eval_ex.eval_expr exp prog |> value_to_output
          | Assign(name, exp) -> Hashtbl.add prog.variables name exp
          | Func_Def(name, exp) -> print_endline "Function definition not implemented!"
    in match prog.program with
      | [] -> ()
      | h::r -> interpret_helper h prog; interpret {prog with program=r; ip=prog.ip+1}
