
open Base
open Ast
open Utils
open Stdio

module Range_impl = struct
    
    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 3 "range"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value = 
        let args = Hash_utils.get_variable state "__args" in
        let get_range (args: int list) : (int * int * int) =
            match List.length args with
              | 1 -> (0, (List.nth_exn args 0), 1)
              | 2 -> (List.nth_exn args 0, (List.nth_exn args 1), 1)
              | 3 -> (List.nth_exn args 0, (List.nth_exn args 1), (List.nth_exn args 2))
              | _ -> print_endline "TypeError: range() takes at most 3 positional arguments but 4 were given"; (0, 0, 0)
        in let rec convert_to_intlist (args: value list) : (int list, string) Result.t =
               match args with
                 | [] -> Ok([])
                 | h::r ->  match h with 
                            | IntV(x) -> let res = convert_to_intlist r in
                                        let h_res = Ok([x]) in
                                        (match (h_res, res) with
                                          | (Ok(x1), Ok(x2)) -> Ok(x1@x2)
                                          | (Error(x1),_) -> Error(x1)
                                          | (_, Error(x2)) -> Error(x2))
                            | _ -> Error("TypeError: range() takes integer arguments but was given")
         in match args with
          | Some(Value(ListV(arg_list))) ->
            (match convert_to_intlist arg_list with
              | Ok(x) -> let (start, stop, step) = get_range x
                         in ListV(List.init (stop - start) ~f:(fun i -> IntV(start + (i * step))))
              | Error(x) -> Exception(x)
            )
          | _ -> Exception("TypeError: range() takes at least 1 positional argument but 0 were given")
end

let load_impls (state: program_state) : unit = 
    (* Load standard library functions implemented in this module. *)
    Utils.Hash_utils.add_variable state "range" (Value(Function(Func_Opq(Range_impl.f), Range_impl.f_on, Range_impl.f_off)));
