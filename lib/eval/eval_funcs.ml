
open Ast

let eval_print x =
    match x with
      | IntV x -> print_int x; print_newline(); Ntwo 
      | FloatV x -> print_float x; print_newline(); Ntwo
      | StringV x -> print_string x; print_newline(); Ntwo
      | BoolV x -> print_string (if x then "True" else "False"); print_newline(); Ntwo
      | Exception x -> print_string x; print_newline(); Ntwo
      | Ntwo -> print_string "None"; print_newline(); Ntwo
      | Function f -> print_string "Function"; print_newline(); Ntwo
