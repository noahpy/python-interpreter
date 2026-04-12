open Base
open Stdio
open Ast
open Utils


module Print_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 (-1) "print"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value = 
        (* Implementation of python print function.*)
        let rec assemble_strings (args: value list) : (string, string) Result.t = 
            match args with
              | [] -> Ok("")
              | h::r -> let res = assemble_strings r in
                        let h_res = (match h with Exception(x) -> Error(x) | _ -> Ok(value_to_str h)) in
                        match (h_res, res) with
                          | (Ok(x1), Ok(x2)) -> Ok(x1^" "^x2)
                          | (Error(x1), Error(x2)) -> Error(x1^"\nand\n"^x2)
                          | (Error(x1), _) -> Error(x1)
                          | (_, Error(x2)) -> Error(x2)
        in let args_exp = Hash_utils.get_variable state "__args"
        in match args_exp with
          | Some(Value(ListV(args))) ->
             (match assemble_strings args with
              | Ok(x) -> print_endline x; Ntwo;
              | Error(x) -> Exception(x)
             )
          | Some(_) -> Exception("TypeError: print only accepts list value in __args!")
          | None -> Exception("__args not found!")
end


module Input_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 1 "input"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value = 
        (* Implementation of python input function.*)
        let args_exp = Hash_utils.get_variable state "__args"
        in match args_exp with
          | Some(Value(ListV(args))) -> Out_channel.output_string stdout (values_to_str args);
                                        Out_channel.flush stdout;
                                        (match In_channel.input_line stdin with
                                          | Some(x) -> StringV(x)
                                          | None -> Exception("Failed to read from stdin.")
                                        )
          | Some(_) -> Exception("TypeError: input only accepts list value in __args!")
          | None -> Exception("__args not found!")

end


module Print_ir_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 0 "print_ir"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value = 
        print_s (sexp_of_program_state state); Ntwo;
end

module Print_vars_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 (-1) "print_vars"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value = 
        let tbl = Hashtbl.create (module String) in
        let args_exp = Hash_utils.get_variable state "__args"
        in match args_exp with
          | Some(Value(ListV([]))) -> 
                print_s [%sexp (state.variables: (string, expr list) Hashtbl.t)]; Ntwo;
          | Some(Value(ListV(args))) ->
                List.iter args ~f:(fun arg -> match arg with StringV(s) -> Hashtbl.set tbl ~key:s ~data:(Hashtbl.find_multi state.variables s) | _ -> ());
                print_s [%sexp (tbl: (string, expr list) Hashtbl.t)]; Ntwo;
          | _ -> Exception("__args not found!")
end

let load_impls (state: program_state) : unit = 
    (* Load standard library functions implemented in this module. *)
    Utils.Hash_utils.add_variable state "print" (Value(Function(Func_Opq(Print_impl.f), Print_impl.f_on, Print_impl.f_off)));
    Utils.Hash_utils.add_variable state "input" (Value(Function(Func_Opq(Input_impl.f), Input_impl.f_on, Input_impl.f_off)));
    Utils.Hash_utils.add_variable state "print_ir" (Value(Function(Func_Opq(Print_ir_impl.f), Print_ir_impl.f_on, Print_ir_impl.f_off)));
    Utils.Hash_utils.add_variable state "print_vars" (Value(Function(Func_Opq(Print_vars_impl.f), Print_vars_impl.f_on, Print_vars_impl.f_off)));
