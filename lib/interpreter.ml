
open Base
open Ast
open Stdio

let init_program_state ?(var_size: int=1) ?(load_stdlib: bool=false) (lines: statement list) : program_state =
    let varH = Hashtbl.create ~size:var_size (module String) in 
    let p = {program = lines; ip = 0; variables = varH; local_variables = Hashtbl.create (module String)} in
    if load_stdlib then (Python_stdlib.Load.load_impls p; p) else p


let interpret_ir ?(print_values:bool=false) (prog: program_state) : unit = 
    let res = Eval.Eval_ex.eval_program prog in 
    match print_values with
      | true -> Ast.value_to_output res
      | false -> ()

let interpret ?(file_name:string="") ?(print_values:bool=false) ?(load_stdlib:bool=false) 
              ?(print_ir:bool=false) ?(interpret_string:bool=false) (): unit =
    let lexbuf = if interpret_string then
                    Lexing.from_string file_name
                else 
                    let channel = match file_name with "" -> stdin | _ -> In_channel.create file_name in
                    Lexing.from_channel channel 
                in
    let state = Indent_buffer.create () in
    try
        let lines: (statement list) = Parser.program (Indent_buffer.next_token state) lexbuf in
        let p = init_program_state ~load_stdlib:load_stdlib lines in
        if print_ir then  
            (print_endline "Program IR:";
            print_s (sexp_of_program_state p))
        else ();
        interpret_ir ~print_values:print_values p
    with
      | Failure msg ->
        eprintf "Error: %s\n" msg;
      | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        eprintf "Parse error at line %d, column %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
