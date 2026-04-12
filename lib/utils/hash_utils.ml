
open Base
open Ast


let add_variable (prog: program_state) (name: string) (value: expr) : unit =
    Hashtbl.add_multi prog.variables ~key:name ~data:value

let get_variable (prog: program_state) (name: string) : expr option =
     Hashtbl.find_multi prog.variables name |> List.hd

let get_nth_variable (prog: program_state) (name: string) (i: int): expr option = 
    let l = Hashtbl.find_multi prog.variables name in List.nth l i

let get_nth_variable_exn (prog: program_state) (name: string) (i: int): expr = 
    let l = Hashtbl.find_multi prog.variables name in List.nth_exn l i

let remove_variable (prog: program_state) (name: string) : unit =
    Hashtbl.remove_multi prog.variables name

let reset_variable (prog: program_state) (name: string) (value: expr) : unit =
    Hashtbl.set prog.variables ~key:name ~data:[value]

let replace_variable (prog: program_state) (name: string) (value: expr) : unit =
    match Hashtbl.find_multi prog.variables name with
      | [] -> reset_variable prog name value
      | h::r -> Hashtbl.set prog.variables ~key:name ~data:(value::r)

let replace_nth_variable (prog: program_state) (name: string) (index: int) (value: expr) : unit =
    match Hashtbl.find_multi prog.variables name with
      | [] -> reset_variable prog name value
      | l -> Hashtbl.set prog.variables ~key:name ~data:(List.mapi l ~f:(fun i v -> if i = index then value else v))

let add_local_variable (prog: program_state) (name: string) (value: expr) : unit =
    match (Hashtbl.find prog.local_variables name) with
      | Some(v) -> replace_variable prog name value
      | None -> add_variable prog name value; Hashtbl.set prog.local_variables ~key:name ~data:true

let remove_local_variabels (prog: program_state) : unit =
    Hashtbl.iter_keys prog.local_variables ~f:(fun key -> remove_variable prog key)

let get_var_stacklen (prog: program_state) (name: string) : int = 
    Hashtbl.find_multi prog.variables name |> List.length


let rec resolve_reference (name: string) (index: int) (prog: program_state) : (string * int) =
    let adjusted_i = (get_var_stacklen prog name) - index in
    let res = get_nth_variable_exn prog name adjusted_i in
    match res with
      | Value(RefV(new_name, i)) ->
              resolve_reference new_name i prog
      | _ -> (name, adjusted_i)

