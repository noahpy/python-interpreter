
open Base
open Ast
open Utils


(* Helper: extract a numeric value as a float. Returns None for non-numeric values. *)
let to_float_opt (v: value) : float option =
    match v with
      | IntV x -> Some(Float.of_int x)
      | FloatV x -> Some(x)
      | BoolV x -> Some(if x then 1.0 else 0.0)
      | _ -> None

(* Helper: compare two numeric values, returning an int ordering. *)
let compare_num (a: value) (b: value) : (int, string) Result.t =
    match to_float_opt a, to_float_opt b with
      | Some(fa), Some(fb) -> Ok(Float.compare fa fb)
      | _ -> Error("TypeError: comparison not supported between instances of '" ^ value_to_str a ^ "' and '" ^ value_to_str b ^ "'")

(* Helper: find min/max of a list of values. [want_greater] picks max when true, min otherwise. *)
let find_extremum (vals: value list) (want_greater: bool) (name: string) : value =
    match vals with
      | [] -> Exception("ValueError: " ^ name ^ "() arg is an empty sequence")
      | h::r ->
        let result = List.fold r ~init:(Ok h) ~f:(fun acc x ->
            match acc with
              | Error(_) -> acc
              | Ok(cur) ->
                (match x with
                  | Exception(msg) -> Error(msg)
                  | _ ->
                    (match compare_num cur x with
                      | Ok(c) ->
                        if want_greater then
                            (if c < 0 then Ok(x) else Ok(cur))
                        else
                            (if c > 0 then Ok(x) else Ok(cur))
                      | Error(msg) -> Error(msg)
                    )
                )
        ) in
        (match result with
          | Ok(v) -> v
          | Error(msg) -> Exception(msg))


module Abs_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 1 "abs"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        (* Implementation of python abs function. *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([arg]))) ->
            (match arg with
              | IntV(x) -> IntV(abs x)
              | FloatV(x) -> FloatV(Float.abs x)
              | BoolV(x) -> IntV(if x then 1 else 0)
              | _ -> Exception("TypeError: bad operand type for abs(): '" ^ value_to_str arg ^ "'")
            )
          | _ -> Exception("TypeError: abs() takes exactly one argument")
end


module Max_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 (-1) "max"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        (* Implementation of python max function. Accepts either a single iterable
           or multiple positional arguments. *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([ListV(lst)]))) -> find_extremum lst true "max"
          | Some(Value(ListV([single]))) ->
            Exception("TypeError: '" ^ value_to_str single ^ "' object is not iterable")
          | Some(Value(ListV(lst))) -> find_extremum lst true "max"
          | _ -> Exception("TypeError: max expected at least 1 argument, got 0")
end


module Min_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 (-1) "min"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        (* Implementation of python min function. Accepts either a single iterable
           or multiple positional arguments. *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([ListV(lst)]))) -> find_extremum lst false "min"
          | Some(Value(ListV([single]))) ->
            Exception("TypeError: '" ^ value_to_str single ^ "' object is not iterable")
          | Some(Value(ListV(lst))) -> find_extremum lst false "min"
          | _ -> Exception("TypeError: min expected at least 1 argument, got 0")
end


module Pow_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 2 3 "pow"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let rec int_pow (b: int) (e: int) : int =
        if e <= 0 then 1
        else if e % 2 = 0 then int_pow (b * b) (e / 2)
        else b * (int_pow (b * b) (e / 2))

    let f (state: program_state) : value =
        (* Implementation of python pow function. Supports pow(base, exp) and
           pow(base, exp, mod) for integer arguments. *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([base; expn]))) ->
            (match base, expn with
              | IntV(b), IntV(e) when e >= 0 -> IntV(int_pow b e)
              | _ ->
                (match to_float_opt base, to_float_opt expn with
                  | Some(fb), Some(fe) -> FloatV(Float.( ** ) fb fe)
                  | _ -> Exception("TypeError: unsupported operand type(s) for pow(): '"
                                    ^ value_to_str base ^ "' and '" ^ value_to_str expn ^ "'")
                )
            )
          | Some(Value(ListV([base; expn; modv]))) ->
            (match base, expn, modv with
              | _, _, IntV(0) -> Exception("ValueError: pow() 3rd argument cannot be 0")
              | IntV(b), IntV(e), IntV(m) when e >= 0 -> IntV((int_pow b e) % m)
              | _ -> Exception("TypeError: pow() 3rd argument not allowed unless all arguments are non-negative integers")
            )
          | _ -> Exception("TypeError: pow() takes 2 or 3 arguments")
end


module Sqrt_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 1 "sqrt"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let f (state: program_state) : value =
        (* Implementation of python sqrt function. *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([arg]))) ->
            (match arg with
              | Exception(msg) -> Exception(msg)
              | _ ->
                (match to_float_opt arg with
                  | Some(x) when Float.(x < 0.0) -> Exception("ValueError: math domain error")
                  | Some(x) -> FloatV(Float.sqrt x)
                  | None -> Exception("TypeError: must be real number, not '" ^ value_to_str arg ^ "'")
                )
            )
          | _ -> Exception("TypeError: sqrt() takes exactly one argument")
end


module Sum_impl = struct

    let f_on:func_oncall = Helper.Helper_Generator.generate_fon_args 1 2 "sum"

    let f_off:func_offcall = Helper.Helper_Generator.generate_foff_args ()

    let rec do_sum (vals: value list) (acc: value) : value =
        match vals with
          | [] -> acc
          | h::r ->
            (match acc, h with
              | Exception(_), _ -> acc
              | _, Exception(msg) -> Exception(msg)
              | IntV(a), IntV(b) -> do_sum r (IntV(a + b))
              | IntV(a), FloatV(b) -> do_sum r (FloatV(Float.of_int a +. b))
              | FloatV(a), IntV(b) -> do_sum r (FloatV(a +. Float.of_int b))
              | FloatV(a), FloatV(b) -> do_sum r (FloatV(a +. b))
              | IntV(a), BoolV(b) -> do_sum r (IntV(a + (if b then 1 else 0)))
              | FloatV(a), BoolV(b) -> do_sum r (FloatV(a +. (if b then 1.0 else 0.0)))
              | BoolV(a), IntV(b) -> do_sum r (IntV((if a then 1 else 0) + b))
              | BoolV(a), FloatV(b) -> do_sum r (FloatV((if a then 1.0 else 0.0) +. b))
              | BoolV(a), BoolV(b) -> do_sum r (IntV((if a then 1 else 0) + (if b then 1 else 0)))
              | _ -> Exception("TypeError: unsupported operand type(s) for +: '"
                                ^ value_to_str acc ^ "' and '" ^ value_to_str h ^ "'")
            )

    let f (state: program_state) : value =
        (* Implementation of python sum function. sum(iterable[, start]). *)
        let args = Hash_utils.get_variable state "__args" in
        match args with
          | Some(Value(ListV([ListV(lst)]))) -> do_sum lst (IntV 0)
          | Some(Value(ListV([ListV(lst); start]))) -> do_sum lst start
          | Some(Value(ListV([single]))) ->
            Exception("TypeError: 'sum()' first argument must be iterable, not '" ^ value_to_str single ^ "'")
          | Some(Value(ListV([single; _]))) ->
            Exception("TypeError: 'sum()' first argument must be iterable, not '" ^ value_to_str single ^ "'")
          | _ -> Exception("TypeError: sum() takes 1 or 2 arguments")
end


let load_impls (state: program_state) : unit =
    (* Load standard library functions implemented in this module. *)
    Utils.Hash_utils.add_variable state "abs" (Value(Function(Func_Opq(Abs_impl.f), Abs_impl.f_on, Abs_impl.f_off)));
    Utils.Hash_utils.add_variable state "max" (Value(Function(Func_Opq(Max_impl.f), Max_impl.f_on, Max_impl.f_off)));
    Utils.Hash_utils.add_variable state "min" (Value(Function(Func_Opq(Min_impl.f), Min_impl.f_on, Min_impl.f_off)));
    Utils.Hash_utils.add_variable state "pow" (Value(Function(Func_Opq(Pow_impl.f), Pow_impl.f_on, Pow_impl.f_off)));
    Utils.Hash_utils.add_variable state "sqrt" (Value(Function(Func_Opq(Sqrt_impl.f), Sqrt_impl.f_on, Sqrt_impl.f_off)));
    Utils.Hash_utils.add_variable state "sum" (Value(Function(Func_Opq(Sum_impl.f), Sum_impl.f_on, Sum_impl.f_off)));
