
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
                            | x -> Error("TypeError: range() takes integer arguments but was given "^  Sexp.to_string (sexp_of_value x))
         in match args with
          | Some(Value(ListV(arg_list))) ->
            (match convert_to_intlist arg_list with
              | Ok(x) -> let (start, stop, step) = get_range x
                         in let len = if step > 0 then max 0 ((stop - start) / step + 1) 
                                                  else max 0 ((start - stop) / -step + 1)
                         in ListV(List.init len ~f:(fun i -> IntV(start + (i * step))))
              | Error(x) -> Exception(x)
            )
          | _ -> Exception("TypeError: range() takes at least 1 positional argument but 0 were given")
end

module Int_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 1 "int"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([arg]))) ->
            (match arg with
              | IntV(_) -> arg
              | FloatV(x) -> IntV(Float.to_int x)
              | StringV(x) -> (match Int.of_string_opt x with
                                | Some(i) -> IntV(i)
                                | None -> Exception("ValueError: invalid literal for int() with base 10: '" ^ x ^ "'"))
              | BoolV(x) -> IntV(if x then 1 else 0)
              | _ -> Exception("TypeError: int() argument must be a string or a number, not '" ^ value_to_str arg ^ "'")
            )
          | _ -> Exception("TypeError: int() takes exactly one argument")
end

module Str_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 1 "str"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([]))) -> StringV("")
          | Some(Value(ListV([arg]))) -> StringV(value_to_str arg)
          | _ -> Exception("TypeError: str() takes at most 1 argument")
end

module Float_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 1 "float"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([arg]))) ->
            (match arg with
              | FloatV(_) -> arg
              | IntV(x) -> FloatV(Float.of_int x)
              | StringV(x) -> (match Float.of_string_opt x with
                                | Some(f) -> FloatV(f)
                                | None -> Exception("ValueError: could not convert string to float: '" ^ x ^ "'"))
              | BoolV(x) -> FloatV(if x then 1.0 else 0.0)
              | _ -> Exception("TypeError: float() argument must be a string or a number, not '" ^ value_to_str arg ^ "'")
            )
          | _ -> Exception("TypeError: float() takes exactly one argument")
end

module List_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 0 1 "list"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([]))) -> ListV([])
          | Some(Value(ListV([arg]))) ->
            (match arg with
              | ListV(_) -> arg
              | StringV(x) -> ListV(List.map (String.to_list x) ~f:(fun c -> StringV(String.of_char c)))
              | _ -> Exception("TypeError: '" ^ value_to_str arg ^ "' object is not iterable")
            )
          | _ -> Exception("TypeError: list() takes at most 1 argument")
end

let load_impls (state: program_state) : unit =
    (* Load standard library functions implemented in this module. *)
    Utils.Hash_utils.add_variable state "range" (Value(Function(Func_Opq(Range_impl.f), Range_impl.f_on, Range_impl.f_off)));
    Utils.Hash_utils.add_variable state "int" (Value(Function(Func_Opq(Int_impl.f), Int_impl.f_on, Int_impl.f_off)));
    Utils.Hash_utils.add_variable state "str" (Value(Function(Func_Opq(Str_impl.f), Str_impl.f_on, Str_impl.f_off)));
    Utils.Hash_utils.add_variable state "float" (Value(Function(Func_Opq(Float_impl.f), Float_impl.f_on, Float_impl.f_off)));
    Utils.Hash_utils.add_variable state "list" (Value(Function(Func_Opq(List_impl.f), List_impl.f_on, List_impl.f_off)));
