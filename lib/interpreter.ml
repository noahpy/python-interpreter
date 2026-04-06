
open Base
open Ast

let init_program_state ?(var_size: int=1) ?(load_stdlib: bool=false) (lines: statement list) : program_state =
    let varH = Hashtbl.create ~size:var_size (module String) in 
    let p = {program = lines; ip = 0; variables = varH;} in
    if load_stdlib then (Python_stdlib.Load.load_impls p; p) else p


let interpret_top (prog: program_state) : unit = 
    match Eval.Eval_ex.interpret prog with _ -> ()
