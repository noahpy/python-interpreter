open! Base
open Python_interpreter
open Stdio
open Python_interpreter.Ast
open Python_interpreter.Interpreter
module Eval = Python_interpreter.Eval.Eval_ex
module Utils = Python_interpreter.Utils

let%expect_test "interpret print after self-referencing variable assignment" = 
    let line1 = Assign("x", Bin_Exp(Value(FloatV 10.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line2 = Assign("y", Var_Ref("x")) in
    let line3 = Assign("x", Var_Ref("y")) in
    let line4 = Expr(Func_App("print", [Value(IntV 10); Var_Ref("x")])) in
    let p = Interpreter.init_program_state [line1; line2; line3; line4] in
    Python_interpreter.Python_stdlib.Io.load_impls p;
    interpret p;
    [%expect {| 10 1.6666666666666667 |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program
        ((Assign
          (x
           (Bin_Exp
            ((Value (FloatV 10)) Div
             (Bin_Exp ((Value (IntV 2)) Mul (Value (IntV 3))))))))
         (Assign (y (Var_Ref x))) (Assign (x (Var_Ref y)))
         (Expr (Func_App (print ((Value (IntV 10)) (Var_Ref x)))))))
       (ip 0)
       (variables
        ((input ((Value (Function (<opaque> <opaque> <opaque>)))))
         (print ((Value (Function (<opaque> <opaque> <opaque>)))))
         (x ((Value (FloatV 1.6666666666666667))))
         (y
          ((Bin_Exp
            ((Value (FloatV 10)) Div
             (Bin_Exp ((Value (IntV 2)) Mul (Value (IntV 3)))))))))))
      |}]

let%expect_test "interpret print after defining custom add function with overlapping local variable names" =
    let f_on args state =
        Utils.Hash_utils.add_variable state "x" (Value(List.nth_exn args 0));
        Utils.Hash_utils.add_variable state "y" (Value(List.nth_exn args 1));
        Ok()
    in let f state =
        Eval.eval_expr (Bin_Exp(Var_Ref("x"), Add, Var_Ref("y"))) state
    in let f_off state =
        Utils.Hash_utils.remove_variable state "x";
        Utils.Hash_utils.remove_variable state "y";
        Ok()
    in let line1 = Assign("y", Value(IntV(1))) in
    let line2 = Assign("x", Value(IntV(2))) in
    let line3 = Func_Def("add", f, f_on, f_off)in
    let line4 = Assign("res", Func_App("add", [Var_Ref("y"); Var_Ref("x")])) in
    let line5 = Expr(Func_App("print", [Var_Ref("res"); Var_Ref("x"); Var_Ref("y")])) in
    let lines = [line1; line2; line3; line4; line5] in 
    let p = init_program_state lines in
    Python_interpreter.Python_stdlib.Io.load_impls p;
    interpret p;
    [%expect {| 3 2 1 |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program
        ((Assign (y (Value (IntV 1)))) (Assign (x (Value (IntV 2))))
         (Func_Def (add <opaque> <opaque> <opaque>))
         (Assign (res (Func_App (add ((Var_Ref y) (Var_Ref x))))))
         (Expr (Func_App (print ((Var_Ref res) (Var_Ref x) (Var_Ref y)))))))
       (ip 0)
       (variables
        ((add ((Value (Function (<opaque> <opaque> <opaque>)))))
         (input ((Value (Function (<opaque> <opaque> <opaque>)))))
         (print ((Value (Function (<opaque> <opaque> <opaque>)))))
         (res ((Value (IntV 3)))) (x ((Value (IntV 2)))) (y ((Value (IntV 1)))))))
      |}]
