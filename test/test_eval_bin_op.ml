
open Base
open Stdio
open Python_interpreter
open Python_interpreter.Ast

(* --- Test Helper --- *)


let run_test v1 op v2 =
    (* Assuming Interpreter.init_program_state exists in your project scope *)
    let p = Interpreter.init_program_state [] in
    let r = Eval.Eval_ex.eval_bin_op (Value v1) op (Value v2) p in
    
    (* Manual Sexp to String conversion for Stdio.printf *)
    let v1_s = Sexp.to_string (sexp_of_value v1) in
    let op_s = Sexp.to_string (sexp_of_bin_op op) in
    let v2_s = Sexp.to_string (sexp_of_value v2) in
    let res_s = Sexp.to_string (sexp_of_value r) in
    let state_s = Sexp.to_string (sexp_of_program_state p) in

    printf "Input: %s %s %s\n" v1_s op_s v2_s;
    printf "Result: %s\n" res_s;
    printf "State: %s\n" state_s;
    print_endline "-----------------------"

(* --- Arithmetic Tests --- *)

let arithmetic_ops = [Add; Sub; Mul; Div]

let%expect_test "eval_bin_op: integer arithmetic" =
    let v1 = IntV 10 in
    let v2 = IntV 2 in
    List.iter arithmetic_ops ~f:(fun op -> run_test v1 op v2);
    run_test (IntV 10) Div (IntV 0);
    [%expect {|
      Input: (IntV 10) Add (IntV 2)
      Result: (IntV 12)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub (IntV 2)
      Result: (IntV 8)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul (IntV 2)
      Result: (IntV 20)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (IntV 2)
      Result: (IntV 5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (IntV 0)
      Result: (Exception"ZeroDivisionError: division by zero")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: float arithmetic" =
    let v1 = FloatV 10.0 in
    let v2 = FloatV 2.5 in
    List.iter arithmetic_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (FloatV 10) Add (FloatV 2.5)
      Result: (FloatV 12.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 10) Sub (FloatV 2.5)
      Result: (FloatV 7.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 10) Mul (FloatV 2.5)
      Result: (FloatV 25)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 10) Div (FloatV 2.5)
      Result: (FloatV 4)
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}];
    run_test (FloatV 10.) Div (FloatV 0.);
    [%expect {|
      Input: (FloatV 10) Div (FloatV 0)
      Result: (Exception"ZeroDivisionError: float division by zero")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: string arithmetic" =
    let v1 = StringV "hello" in
    let v2 = StringV "world" in
    List.iter arithmetic_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (StringV hello) Add (StringV world)
      Result: (StringV helloworld)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV hello) Sub (StringV world)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV hello) Mul (StringV world)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV hello) Div (StringV world)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: boolean arithmetic" =
    let v1 = BoolV true in
    let v2 = BoolV false in
    List.iter arithmetic_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (BoolV true) Add (BoolV false)
      Result: (Exception"Operation not supported for type bool")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub (BoolV false)
      Result: (Exception"Operation not supported for type bool")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul (BoolV false)
      Result: (Exception"Operation not supported for type bool")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div (BoolV false)
      Result: (Exception"Operation not supported for type bool")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]


let%expect_test "eval_bin_op: list arithmetic" =
    let v1 = ListV [IntV 1]in
    let v2 = ListV [FloatV 2.5] in
    List.iter arithmetic_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (ListV((IntV 1))) Add (ListV((FloatV 2.5)))
      Result: (ListV((IntV 1)(FloatV 2.5)))
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Sub (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Mul (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Div (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

(* --- Comparison Tests (Same Type) --- *)

let comparison_ops = [Less; Greater; Leq; Geq; Equal; Neq]

let%expect_test "eval_bin_op: integer comparisons" =
    let v1 = IntV 5 in
    let v2 = IntV 10 in
    List.iter comparison_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (IntV 5) Less (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 5) Greater (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 5) Leq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 5) Geq (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 5) Equal (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 5) Neq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: float comparisons" =
    let v1 = FloatV 1.1 in
    let v2 = FloatV 1.1 in
    List.iter comparison_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (FloatV 1.1) Less (FloatV 1.1)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 1.1) Greater (FloatV 1.1)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 1.1) Leq (FloatV 1.1)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 1.1) Geq (FloatV 1.1)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 1.1) Equal (FloatV 1.1)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 1.1) Neq (FloatV 1.1)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: string comparisons" =
    let v1 = StringV "apple" in
    let v2 = StringV "banana" in
    List.iter comparison_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (StringV apple) Less (StringV banana)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV apple) Greater (StringV banana)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV apple) Leq (StringV banana)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV apple) Geq (StringV banana)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV apple) Equal (StringV banana)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV apple) Neq (StringV banana)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

let%expect_test "eval_bin_op: list comparisons" =
    let v1 = ListV [IntV 1]in
    let v2 = ListV [FloatV 2.5] in
    List.iter comparison_ops ~f:(fun op -> run_test v1 op v2);
    [%expect {|
      Input: (ListV((IntV 1))) Less (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Greater (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Leq (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Geq (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Equal (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1))) Neq (ListV((FloatV 2.5)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

(* --- Logical Tests --- *)

let logical_ops = [And; Or]

let%expect_test "eval_bin_op: logical operations" =
    let cases = [
        (BoolV true, And, BoolV false);
        (BoolV true, Or, BoolV false);
        (BoolV true, And, BoolV true);
        (BoolV true, Or, BoolV true);
        (IntV 1, And, IntV 0);
        (IntV 1, Or, IntV 0);
        (IntV 1, And, IntV 1);
        (IntV 1, Or, IntV 1);
    ] in
    List.iter cases ~f:(fun (v1, op, v2) -> run_test v1 op v2);
    [%expect {|
      Input: (BoolV true) And (BoolV false)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (BoolV false)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 1) And (IntV 0)
      Result: (IntV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 1) Or (IntV 0)
      Result: (IntV 0)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 1) And (IntV 1)
      Result: (IntV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 1) Or (IntV 1)
      Result: (IntV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]

(* --- Cross-Type Tests --- *)

let all_ops = [
    Add; Mul; Sub; Div; 
    And; Or; 
    Less; Greater; Leq; Geq; Equal; Neq
]

let sample_values = [
    IntV 10;
    FloatV 2.5;
    StringV "abc";
    BoolV true;
    Ntwo;
    ListV [IntV 1; FloatV 2.5; StringV "abc"; BoolV true];
]

(** Helper to check if two values have different variant types *)
let types_differ v1 v2 =
    match v1, v2 with
    | IntV _, IntV _ -> false
    | FloatV _, FloatV _ -> false
    | StringV _, StringV _ -> false
    | BoolV _, BoolV _ -> false
    | ListV _, ListV _ -> false
    | Ntwo, Ntwo -> false
    | _ -> true

let%expect_test "eval_bin_op: all cross-type combinations" =
    List.iter all_ops ~f:(fun op ->
        List.iter sample_values ~f:(fun v1 ->
            List.iter sample_values ~f:(fun v2 ->
                if types_differ v1 v2 then
                    run_test v1 op v2
            )
        )
    );
    [%expect {|
      Input: (IntV 10) Add (FloatV 2.5)
      Result: (FloatV 12.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Add (StringV abc)
      Result: (StringV 10abc)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Add (BoolV true)
      Result: (IntV 11)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Add Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Add (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Add (IntV 10)
      Result: (FloatV 12.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Add (StringV abc)
      Result: (StringV 2.5abc)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Add (BoolV true)
      Result: (FloatV 3.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Add Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Add (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Add (IntV 10)
      Result: (StringV abc10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Add (FloatV 2.5)
      Result: (StringV abc2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Add (BoolV true)
      Result: (StringV abctrue)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Add Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Add (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Add (IntV 10)
      Result: (IntV 11)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Add (FloatV 2.5)
      Result: (FloatV 3.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Add (StringV abc)
      Result: (StringV trueabc)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Add Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Add (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Add (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Add (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Add (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Add (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Add (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Add (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Add (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Add (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Add (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Add Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul (FloatV 2.5)
      Result: (FloatV 25)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul (BoolV true)
      Result: (IntV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Mul (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Mul (IntV 10)
      Result: (FloatV 25)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Mul (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Mul (BoolV true)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Mul Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Mul (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Mul (IntV 10)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Mul (FloatV 2.5)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Mul (BoolV true)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Mul Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Mul (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul (IntV 10)
      Result: (IntV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul (FloatV 2.5)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Mul (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Mul (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Mul (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Mul (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Mul (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Mul (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Mul (IntV 10)
      Result: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)(IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Mul (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Mul (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Mul (BoolV true)
      Result: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Mul Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub (FloatV 2.5)
      Result: (FloatV 7.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub (BoolV true)
      Result: (IntV 9)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Sub (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Sub (IntV 10)
      Result: (FloatV -7.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Sub (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Sub (BoolV true)
      Result: (FloatV 1.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Sub Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Sub (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Sub (IntV 10)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Sub (FloatV 2.5)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Sub (BoolV true)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Sub Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Sub (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub (IntV 10)
      Result: (IntV -9)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub (FloatV 2.5)
      Result: (FloatV -1.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Sub (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Sub (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Sub (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Sub (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Sub (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Sub (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Sub (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Sub (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Sub (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Sub (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Sub Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (FloatV 2.5)
      Result: (FloatV 4)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (BoolV true)
      Result: (IntV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Div (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Div (IntV 10)
      Result: (FloatV 0.25)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Div (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Div (BoolV true)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Div Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Div (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Div (IntV 10)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Div (FloatV 2.5)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Div (BoolV true)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Div Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Div (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div (IntV 10)
      Result: (IntV 0)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div (FloatV 2.5)
      Result: (FloatV 0.4)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Div (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Div (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Div (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Div (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Div (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Div (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Div (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Div (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Div (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Div (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Div Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) And (FloatV 2.5)
      Result: (FloatV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) And (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) And (BoolV true)
      Result: (IntV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) And Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) And (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) And (IntV 10)
      Result: (FloatV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) And (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) And (BoolV true)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) And Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) And (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) And (IntV 10)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) And (FloatV 2.5)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) And (BoolV true)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) And Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) And (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And (IntV 10)
      Result: (IntV 10)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And (FloatV 2.5)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) And (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo And (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo And (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo And (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo And (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo And (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) And (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) And (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) And (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) And (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) And Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Or (FloatV 2.5)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Or (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Or (BoolV true)
      Result: (IntV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Or Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Or (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Or (IntV 10)
      Result: (FloatV 2.5)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Or (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Or (BoolV true)
      Result: (FloatV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Or Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Or (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Or (IntV 10)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Or (FloatV 2.5)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Or (BoolV true)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Or Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Or (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (IntV 10)
      Result: (IntV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (FloatV 2.5)
      Result: (FloatV 1)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (StringV abc)
      Result: (Exception"Operation not supported for type string")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Or (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Or (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Or (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Or (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Or (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Or (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Or (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Or (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Or (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Or (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Or Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Less (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Less (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Less (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Less Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Less (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Less (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Less (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Less (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Less Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Less (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Less (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Less (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Less (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Less Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Less (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Less (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Less (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Less (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Less Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Less (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Less (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Less (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Less (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Less (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Less (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Less (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Less (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Less (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Less (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Less Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Greater (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Greater (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Greater (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Greater Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Greater (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Greater (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Greater (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Greater (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Greater Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Greater (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Greater (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Greater (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Greater (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Greater Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Greater (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Greater (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Greater (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Greater (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Greater Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Greater (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Greater (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Greater (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Greater (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Greater (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Greater (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Greater (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Greater (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Greater (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Greater (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Greater Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Leq (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Leq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Leq (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Leq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Leq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Leq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Leq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Leq (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Leq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Leq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Leq (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Leq (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Leq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Leq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Leq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Leq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Leq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Leq (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Leq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Leq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Leq (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Leq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Leq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Leq (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Leq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Leq (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Leq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Leq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Leq (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Leq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Geq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Geq (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Geq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Geq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Geq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Geq (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Geq (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Geq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Geq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Geq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Geq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Geq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Geq (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Geq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Geq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Geq (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Geq (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Geq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Geq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Geq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Geq (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Geq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Geq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Geq (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Geq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Geq (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Geq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Geq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Geq (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Geq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Equal (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Equal (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Equal (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Equal Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Equal (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Equal (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Equal (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Equal (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Equal Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Equal (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Equal (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Equal (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Equal (BoolV true)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Equal Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Equal (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Equal (IntV 10)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Equal (FloatV 2.5)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Equal (StringV abc)
      Result: (BoolV false)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Equal Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Equal (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Equal (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Equal (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Equal (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Equal (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Equal (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Equal (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Equal (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Equal (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Equal (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Equal Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Neq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Neq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Neq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Neq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (IntV 10) Neq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Neq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Neq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Neq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Neq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (FloatV 2.5) Neq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Neq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Neq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Neq (BoolV true)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Neq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (StringV abc) Neq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Neq (IntV 10)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Neq (FloatV 2.5)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Neq (StringV abc)
      Result: (BoolV true)
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Neq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (BoolV true) Neq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Neq (IntV 10)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Neq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Neq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Neq (BoolV true)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: Ntwo Neq (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true)))
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Neq (IntV 10)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Neq (FloatV 2.5)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Neq (StringV abc)
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Neq (BoolV true)
      Result: (Exception"Operation not supported for type list")
      State: ((program())(ip 0)(variables()))
      -----------------------
      Input: (ListV((IntV 1)(FloatV 2.5)(StringV abc)(BoolV true))) Neq Ntwo
      Result: (Exception"We do not support this implicit type conversion.\nand\n")
      State: ((program())(ip 0)(variables()))
      -----------------------
      |}]
