open! Base
open Python_interpreter
open Stdio
open Python_interpreter.Ast
open Python_interpreter.Interpreter


let%expect_test "eval_expr: eval int value" = 
    let line1 = Assign("x", Bin_Exp(Value(FloatV 10.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line2 = Assign("y", Var_Ref("x")) in
    let line3 = Assign("x", Var_Ref("y")) in
    let line4 = Expr(Func_App("print", [Value(IntV 10); Var_Ref("x")])) in
    let p = Interpreter.init_program_state ~load_stdlib:true [line1; line2; line3; line4] in
    interpret p;
    [%expect {||}];
    print_s [%sexp (p: program_state)];
    [%expect {||}]
