
open Ast


let rec print_impl (args: value list) (state: program_state) : value = 
    (* Implementation of python print function.*)
    let print_helpler (v: value) : unit =
        match v with
          | IntV x -> print_int x; print_string " " 
          | FloatV x -> print_float x; print_string " "
          | StringV x -> print_string x; print_string " "
          | BoolV x -> print_string (if x then "True" else "False"); print_string " "
          | Exception x -> print_string x; print_string " "
          | Ntwo -> print_string "None"; print_string " "
          | Function f -> print_string "Function"; print_string " "
    in List.iter print_helpler args; print_newline(); Ntwo


let load_impls (state: program_state) : unit = 
    (* Load standard library functions implemented in this module. *)
    Hashtbl.add state.variables "print" (Value(Function(print_impl)))
