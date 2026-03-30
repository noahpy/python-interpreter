
open Ast


let print_impl (args: value list) (state: program_state) : value = 
    (* Implementation of python print function.*)
    let print_helpler (v: value) : (string, string) result =
        match v with
          | IntV x -> Ok(string_of_int x)
          | FloatV x -> Ok(string_of_float x)
          | StringV x -> Ok x
          | BoolV x -> Ok(if x then "True" else "False")
          | Exception x -> Error(x)
          | Ntwo -> Ok "None"
          | Function f -> Ok "Function"
    in let rec assemble_strings (args: value list) : (string, string) result = 
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
      | Ok() -> print_string (value_to_str (List.hd args)); StringV (read_line ())
      | Error(x) -> Exception(x)


let load_impls (state: program_state) : unit = 
    (* Load standard library functions implemented in this module. *)
    Hashtbl.add state.variables "print" (Value(Function(print_impl)));
    Hashtbl.add state.variables "input" (Value(Function(input_impl)))
