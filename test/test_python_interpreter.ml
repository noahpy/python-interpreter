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
       (ip 0) (variables ()) (local_variables ()))
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
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))))
       (local_variables ()))
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
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))))
       (local_variables ()))
      [1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 5, 6]
      |}]

let%expect_test "Interpret if/else statement" =
    let program = {|
x = 10
if x > 5:
    print("big")
else:
    print("small")
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| big |}]

let%expect_test "Interpret if/elif/else statement" =
    let program = {|
x = 3
if x == 1:
    print("one")
elif x == 2:
    print("two")
elif x == 3:
    print("three")
else:
    print("other")
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| three |}]

let%expect_test "Interpret while loop" =
    let program = {|
x = 0
while x < 5:
    x = x + 1
print(x)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 5 |}]

let%expect_test "Interpret for loop" =
    let program = {|
total = 0
for i in [1, 2, 3, 4, 5]:
    total = total + i
print(total)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 15 |}]

let%expect_test "Interpret user-defined function" =
    let program = {|
def add(a, b):
    return a + b
result = add(3, 7)
print(result)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 10 |}]

let%expect_test "Interpret nested if in function" =
    let program = {|
def classify(x):
    if x > 0:
        return "positive"
    else:
        return "non-positive"
print(classify(5))
print(classify(-1))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      positive
      non-positive
      |}]

let%expect_test "Interpret for loop with function call" =
    let program = {|
def double(x):
    return x + x
for i in [1, 2, 3]:
    print(double(i))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      2
      4
      6
      |}]

let%expect_test "For loop overwrites variable" =
    let program = {|
i = "should change"
print(i)
total = 0
for i in ["a", "b", "changed!"]:
    pass
print(i)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should change
      changed!
      |}]

let%expect_test "Function call with local var does not overwrite variable" =
    let program = {|
def f(x):
    y = "changed!"
    return x

y = "should not change"
print(f(y))
print(y)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change
      should not change
      |}]

let%expect_test "Function call with local var does not overwrite variable (2)" =
    let program = {|
def f(x):
    y = "should not change"
    print(y)
    def g(x):
        y = "changed!"
        return x
    g(x)
    print(y)
    return x

f(0)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change
      should not change
      |}]

let%expect_test "Function call with local var does not overwrite variable (3)" =
    let program = {|
def g(x):
    y = "changed!"
    return x

def f(x):
    y = "should not change"
    print(y)
    g(x)
    print(y)
    return x

f(0)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change
      should not change
      |}]

let%expect_test "Function call with local var does not overwrite variable (4)" =
    let program = {|
def g(x):
    y = "changed!"
    return x

def f(x):
    return g(x)

y = "should not change"
print(y)
f(0)
print(y)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change
      should not change
      |}]

let%expect_test "Function call with local var does not overwrite variable (5)" =
    let program = {|
def g(x):
    y = "changed!"
    y = "changed again!"
    return x

def f(x):
    return g(x)

y = "should not change"
print(y)
f(0)
print(y)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change
      should not change
      |}]

let%expect_test "Function call with local var does not overwrite variable (6)" =
    let program = {|
def g(x):
    y = "changed!"
    z = "changed again!"
    return x

def f(x):
    return g(x)

y = "should not change 1"
z = "should not change 2"
print(y, z)
f(0)
print(y, z)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      should not change 1 should not change 2
      should not change 1 should not change 2
      |}]

let%expect_test "Local variable should not be available after function call" =
    let program = {|
def f(x):
    y = "changed!"
    return x

f(0)
print(y)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| changed! |}]


let%expect_test "Wrong indentation should create an error" =
    let program = {|
def f(x):
        y = "changed!"
    return x

f(0)
print(y)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| Error: inconsistent indentation |}]
