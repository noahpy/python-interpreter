open! Base
open Stdio
open Python_interpreter.Interpreter

let%expect_test "Interpret simple program" = 
    let program = {|( 5*3) + 10 / (2.1 - 1.34)|} in 
    interpret ~file_name:program ~print_values:true ~load_stdlib:false ~print_ir:true ~interpret_string:true ();
    [%expect {|
      Program IR:
      ((program
        ((Expr
          (Bin_Exp
           ((Bin_Exp ((Value (IntV 5)) Mul (Value (IntV 3)))) Add
            (Bin_Exp
             ((Value (IntV 10)) Div
              (Bin_Exp ((Value (FloatV 2.1)) Sub (Value (FloatV 1.34)))))))))))
       (ip 0) (variables ()))
      None
      |}]

let%expect_test "Interpret program with variables and list" = 
    let program = {|
x = 10 + 3
y = 1.3 * 2
x = x + y
l = [10+4, "wow"+"hu", x] 
print(l)
|} in 
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~print_ir:true ~interpret_string:true ();
    [%expect {|
      Program IR:
      ((program
        ((Assign (x (Bin_Exp ((Value (IntV 10)) Add (Value (IntV 3))))))
         (Assign (y (Bin_Exp ((Value (FloatV 1.3)) Mul (Value (IntV 2))))))
         (Assign (x (Bin_Exp ((Var_Ref x) Add (Var_Ref y)))))
         (Assign
          (l
           (ListE
            ((Bin_Exp ((Value (IntV 10)) Add (Value (IntV 4))))
             (Bin_Exp ((Value (StringV wow)) Add (Value (StringV hu))))
             (Var_Ref x)))))
         (Expr (Func_App (print ((Var_Ref l)))))))
       (ip 0)
       (variables
        ((input ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>))))))))
      [14, wowhu, 15.6]
      |}]

let%expect_test "Interpret program with list arithmetics" = 
    let program = {|
x = 3
l = x * [1, 2, 3] + [4, 5, 6]
print(l)
|} in 
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~print_ir:true ~interpret_string:true ();
    [%expect {|
      Program IR:
      ((program
        ((Assign (x (Value (IntV 3))))
         (Assign
          (l
           (Bin_Exp
            ((Bin_Exp
              ((Var_Ref x) Mul
               (ListE ((Value (IntV 1)) (Value (IntV 2)) (Value (IntV 3))))))
             Add (ListE ((Value (IntV 4)) (Value (IntV 5)) (Value (IntV 6))))))))
         (Expr (Func_App (print ((Var_Ref l)))))))
       (ip 0)
       (variables
        ((input ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>))))))))
      [1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 5, 6]
      |}]
