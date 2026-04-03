open! Base
open Python_interpreter
open Stdio
open Python_interpreter.Ast

(* Evaluation of values *)

let%expect_test "eval_expr: eval int value" = 
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(IntV 1)) p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 1) |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

    
let%expect_test "eval_expr: eval float value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(FloatV 3.141592)) p in
    print_s [%sexp (r1: value)];
    [%expect {| (FloatV 3.141592) |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: eval string value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(StringV "Hello World")) p in
    print_s [%sexp (r1: value)];
    [%expect {| (StringV "Hello World") |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]


let%expect_test "eval_expr: eval bool value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(BoolV true)) p in
    print_s [%sexp (r1: value)];
    [%expect {| (BoolV true) |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: eval exception value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(Exception "Test exception")) p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "Test exception") |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: eval Ntwo value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(Ntwo)) p in
    print_s [%sexp (r1: value)];
    [%expect {| Ntwo |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

