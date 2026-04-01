open! Base
open Python_interpreter
open Stdio
open Python_interpreter.Ast

let%expect_test "eval_expr: eval value" = 
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(IntV 1)) p in
    print_s [%sexp (r1: value)];
    [%expect {| |}];
    print_s [%sexp (p: program_state)];
    [%expect {| |}]

    

