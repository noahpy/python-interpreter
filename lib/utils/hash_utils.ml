
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

let add_local_variable (prog: program_state) (name: string) (value: expr) : unit =
    match (Hashtbl.find prog.local_variables name) with
      | Some(v) -> replace_variable prog name value
      | None -> add_variable prog name value; Hashtbl.set prog.local_variables ~key:name ~data:true

let remove_local_variabels (prog: program_state) : unit =
    Hashtbl.iter_keys prog.local_variables ~f:(fun key -> remove_variable prog key)
