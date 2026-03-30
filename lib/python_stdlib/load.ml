
open Ast

let load_impls (state: program_state) : unit = 
    (* Load standard library functions. *)
    Io.load_impls state;

