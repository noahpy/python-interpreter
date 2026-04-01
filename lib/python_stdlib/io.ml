open Base
open Stdio
open Ast


let print_impl (args: value list) (state: program_state) : value = 
    (* Implementation of python print function.*)
    let print_helpler (v: value) : (string, string) Result.t =
        match v with
          | IntV x -> Ok(Int.to_string x)
          | FloatV x -> Ok(Float.to_string x)
          | StringV x -> Ok x
          | BoolV x -> Ok(if x then "True" else "False")
          | Exception x -> Error(x)
          | Ntwo -> Ok "None"
          | Function f -> Ok "Function"
    in let rec assemble_strings (args: value list) : (string, string) Result.t = 
        match args with
          | [] -> Ok("")
          | h::r -> let res = assemble_strings r in
                    let h_res = print_helpler h in
                    match (h_res, res) with
                      | (Ok(x1), Ok(x2)) -> Ok(x1^" "^x2)
                      | (Error(x1), Error(x2)) -> Error(x1^"\nand\n"^x2)
                      | (Error(x1), _) -> Error(x1)
                      | (_, Error(x2)) -> Error(x2)
    in match assemble_strings args with
      | Ok(x) -> print_endline x; Ntwo;
      | Error(x) -> Exception(x)


let input_impl (args: value list) (state: program_state) : value = 
    (* Implementation of python input function.*)
    match Helper.check_arg_count args 0 1 "input" with
      | Ok() -> print_string (values_to_str args);
                (match In_channel.input_line stdin with
                  | Some(x) -> StringV(x)
                  | None -> Exception("Failed to read from stdin.")
                )
      | Error(x) -> Exception(x)


let load_impls (state: program_state) : unit = 
    (* Load standard library functions implemented in this module. *)
    Hashtbl.set state.variables ~key:"print" ~data:(Value(Function(print_impl)));
    Hashtbl.set state.variables ~key:"input" ~data:(Value(Function(input_impl)))
