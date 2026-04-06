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

let%expect_test "eval_expr: eval list value" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (Value(ListV [IntV 1; FloatV 12.5; StringV "Hello World"])) p in
    print_s [%sexp (r1: value)];
    [%expect {| (ListV ((IntV 1) (FloatV 12.5) (StringV "Hello World"))) |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]


(* Variable evaluation *)

let%expect_test "eval_expr: eval undeclared variable" =
    let p = Interpreter.init_program_state [] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "x") p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "NameError: name 'x' is not defined") |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: eval single variable" =
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 1))] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "x") p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 1) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 1)))))) (ip 0)
       (variables ((x ((Value (IntV 1)))))))
      |}]

let%expect_test "eval_expr: eval multiple variable" =
    let lines = [Assign("x", Value(IntV 1)); Assign("y", Value(IntV 2))] in 
    let p = Interpreter.init_program_state lines in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Bin_Exp(Var_Ref "x", Add, Var_Ref "y")) p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 3) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 1)))) (Assign (y (Value (IntV 2))))))
       (ip 0) (variables ((x ((Value (IntV 1)))) (y ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: eval two variables with one missing" =
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 1))] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Bin_Exp(Var_Ref "x", Add, Var_Ref "y")) p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "NameError: name 'y' is not defined") |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 1)))))) (ip 0)
       (variables ((x ((Value (IntV 1)))))))
      |}]

let%expect_test "eval_expr: eval two variables with both missing" =
    let p = Interpreter.init_program_state [] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Bin_Exp(Var_Ref "x", Add, Var_Ref "y")) p in
    print_s [%sexp (r1: value)];
    [%expect {|
      (Exception
        "NameError: name 'x' is not defined\
       \nand\
       \nNameError: name 'y' is not defined")
      |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: use undeclared variable as function" =
    let p = Interpreter.init_program_state [] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Func_App ("x", [])) p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "NameError: name 'x' is not defined") |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: use declared variable as function" =
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 1))] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Func_App ("x", [])) p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "EvalError: Variable x can not be evalueted with applying.") |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 1)))))) (ip 0)
       (variables ((x ((Value (IntV 1)))))))
      |}]

let%expect_test "eval_expr: eval multiple recursive variables" =
    let lines = [Assign("y", Value(IntV 2)); Assign("x", Var_Ref("y"))] in 
    let p = Interpreter.init_program_state lines in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "x") p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 2) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (y (Value (IntV 2)))) (Assign (x (Var_Ref y))))) (ip 0)
       (variables ((x ((Value (IntV 2)))) (y ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: eval multiple recursive variables: missing" =
    let lines = [Assign("x", Var_Ref("z")); Assign("y", Value(IntV 2))] in 
    let p = Interpreter.init_program_state lines in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "x") p in
    print_s [%sexp (r1: value)];
    [%expect {| (Exception "NameError: name 'z' is not defined") |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Var_Ref z))) (Assign (y (Value (IntV 2)))))) (ip 0)
       (variables
        ((x ((Value (Exception "NameError: name 'z' is not defined"))))
         (y ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: eval circular variable assignment" =
    let lines = [Assign("y", Value(IntV 2)); Assign("x", Var_Ref("y")); Assign("y", Var_Ref("x"))] in 
    let p = Interpreter.init_program_state lines in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "y") p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 2) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program
        ((Assign (y (Value (IntV 2)))) (Assign (x (Var_Ref y)))
         (Assign (y (Var_Ref x)))))
       (ip 0) (variables ((x ((Value (IntV 2)))) (y ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: eval self-referencing variable assignment" =
    let lines = [Assign("x", Value(IntV 2)); Assign("x", Bin_Exp(Var_Ref("x"), Add, Value(IntV 1)))] in 
    let p = Interpreter.init_program_state lines in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref "x") p in
    print_s [%sexp (r1: value)];
    [%expect {| (IntV 3) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program
        ((Assign (x (Value (IntV 2))))
         (Assign (x (Bin_Exp ((Var_Ref x) Add (Value (IntV 1))))))))
       (ip 0) (variables ((x ((Value (IntV 3)))))))
      |}]

(* List evaluation *)

let%expect_test "eval_expr: evaluate list with expressions" =
    let p = Interpreter.init_program_state [] in
    let r1 = Eval.Eval_ex.eval_expr (ListE([Value(IntV 1); Bin_Exp(Value(IntV 2), Add, Value(IntV 1))])) p in
    print_s [%sexp (r1: value)];
    [%expect {| (ListV ((IntV 1) (IntV 3))) |}];
    print_s [%sexp (p: program_state)];
    [%expect {| ((program ()) (ip 0) (variables ())) |}]

let%expect_test "eval_expr: evaluate list with expressions (with variables)" =
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 2))] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (ListE([Value(IntV 1); Bin_Exp(Var_Ref("x"), Add, Value(IntV 1))])) p in
    print_s [%sexp (r1: value)];
    [%expect {| (ListV ((IntV 1) (IntV 3))) |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 2)))))) (ip 0)
       (variables ((x ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: evaluate recursing list" =
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 2))] in
    Interpreter.interpret_top p;
    let r1 = Eval.Eval_ex.eval_expr (ListE([ListE([Value(IntV 1); Bin_Exp(Var_Ref("x"), Mul, ListE([Value(StringV "hello")]))]); (Value(FloatV 3.14))])) p in
    print_s [%sexp (r1: value)];
    [%expect {|
      (ListV
       ((ListV ((IntV 1) (ListV ((StringV hello) (StringV hello))))) (FloatV 3.14)))
      |}];
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program ((Assign (x (Value (IntV 2)))))) (ip 0)
       (variables ((x ((Value (IntV 2)))))))
      |}]

let%expect_test "eval_expr: evaluate list in variable" =
    let line2 = Assign("l", (ListE([ListE([Value(IntV 1); Bin_Exp(Var_Ref("x"), Mul, ListE([Value(StringV "hello")]))]); (Value(FloatV 3.14))]))) in
    let p = Interpreter.init_program_state [Assign("x", Value(IntV 2)); line2] in
    Interpreter.interpret_top p;
    print_s [%sexp (p: program_state)];
    [%expect {|
      ((program
        ((Assign (x (Value (IntV 2))))
         (Assign
          (l
           (ListE
            ((ListE
              ((Value (IntV 1))
               (Bin_Exp ((Var_Ref x) Mul (ListE ((Value (StringV hello))))))))
             (Value (FloatV 3.14))))))))
       (ip 0)
       (variables
        ((l
          ((ListE
            ((ListE
              ((Value (IntV 1))
               (Bin_Exp ((Value (IntV 2)) Mul (ListE ((Value (StringV hello))))))))
             (Value (FloatV 3.14))))))
         (x ((Value (IntV 2)))))))
      |}];
    let r1 = Eval.Eval_ex.eval_expr (Var_Ref("l")) p in
    print_s [%sexp (r1: value)];
    [%expect {|
      (ListV
       ((ListV ((IntV 1) (ListV ((StringV hello) (StringV hello))))) (FloatV 3.14)))
      |}];
    let p2 = {p with program = [Expr(Var_Ref("l"))]} in
    Interpreter.interpret_top p2;
    print_s [%sexp (p2: program_state)];
    [%expect {|
      ((program ((Expr (Var_Ref l)))) (ip 0)
       (variables
        ((l
          ((Value
            (ListV
             ((ListV ((IntV 1) (ListV ((StringV hello) (StringV hello)))))
              (FloatV 3.14))))))
         (x ((Value (IntV 2)))))))
      |}]
