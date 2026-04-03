
open Base
open Ast

let add_variable (prog: program_state) (name: string) (value: expr) : unit =
    Hashtbl.add_multi prog.variables ~key:name ~data:value

let get_variable (prog: program_state) (name: string) : expr option =
     Hashtbl.find_multi prog.variables name |> List.hd

let remove_variable (prog: program_state) (name: string) : unit =
    Hashtbl.remove_multi prog.variables name

let reset_variable (prog: program_state) (name: string) (value: expr) : unit =
    Hashtbl.set prog.variables ~key:name ~data:[value]

let replace_variable (prog: program_state) (name: string) (value: expr) : unit =
    match Hashtbl.find_multi prog.variables name with
      | [] -> reset_variable prog name value
      | h::r -> Hashtbl.set prog.variables ~key:name ~data:(value::r)
