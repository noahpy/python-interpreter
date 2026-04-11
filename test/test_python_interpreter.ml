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
        ((Assign (x (Bin_Exp ((Value (IntV 10)) Add (Value (IntV 3)))) ()))
         (Assign (y (Bin_Exp ((Value (FloatV 1.3)) Mul (Value (IntV 2)))) ()))
         (Assign (x (Bin_Exp ((Var_Ref x) Add (Var_Ref y))) ()))
         (Assign
          (l
           (ListE
            ((Bin_Exp ((Value (IntV 10)) Add (Value (IntV 4))))
             (Bin_Exp ((Value (StringV wow)) Add (Value (StringV hu))))
             (Var_Ref x)))
           ()))
         (Expr (Func_App (print ((Var_Ref l)))))))
       (ip 0)
       (variables
        ((bool ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (float ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (input ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (int ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (len ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (list ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (range ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (str ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))))
       (local_variables ()))
      [14, "wowhu", 15.6]
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
        ((Assign (x (Value (IntV 3)) ()))
         (Assign
          (l
           (Bin_Exp
            ((Bin_Exp
              ((Var_Ref x) Mul
               (ListE ((Value (IntV 1)) (Value (IntV 2)) (Value (IntV 3))))))
             Add (ListE ((Value (IntV 4)) (Value (IntV 5)) (Value (IntV 6))))))
           ()))
         (Expr (Func_App (print ((Var_Ref l)))))))
       (ip 0)
       (variables
        ((bool ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (float ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (input ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (int ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (len ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (list ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (print ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (range ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))
         (str ((Value (Function ((Func_Opq <opaque>) <opaque> <opaque>)))))))
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
    [%expect {|
      Exception at statement 2:
      NameError: name 'y' is not defined
      Error: Program failed.
      |}]


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


let%expect_test "Jibberish should create an error" =
    let program = {|
jeff f(x):
    return x
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| Parse error at line 2, column 6 |}]

let%expect_test "Unallowed syntax should create an error" =
    let program = {|
def def f(x):
    return x
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| Parse error at line 2, column 7 |}]

let%expect_test "Unallowed syntax should create an error (2)" =
    let program = {|
def f(1x):
    return 1x
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| Parse error at line 2, column 7 |}]


let%expect_test "Test range implementation" =
    let program = {|
print(range(10))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] |}]

let%expect_test "Test range implementation (2)" =
    let program = {|
print(range(5, 10))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [5, 6, 7, 8, 9] |}]

let%expect_test "Test range implementation (3)" =
    let program = {|
print(range(5, 10, 2))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [5, 7, 9] |}]

let%expect_test "Test range implementation (4)" =
    let program = {|
print(range(6, 2))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [] |}]

let%expect_test "Test range implementation (5)" =
    let program = {|
print(range(6, 2, -1))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [6, 5, 4, 3, 2] |}]

let%expect_test "Test range implementation (6)" =
    let program = {|
print(range(6, -10, -3))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| [6, 3, 0, -3, -6, -9] |}]

let%expect_test "Test list access" =
    let program = {|
print([1,2,3][0])
print([1,2,3][-1])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      1
      3
      |}]

let%expect_test "Test list access (2)" =
    let program = {|
print([1,2,3][-10])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      Exception at statement 0:
      IndexError: list index out of range
      Error: Program failed.
      |}]

let%expect_test "Test list access (3)" =
    let program = {|
print([1,2,3][10])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      Exception at statement 0:
      IndexError: list index out of range
      Error: Program failed.
      |}]

let%expect_test "Test list access (4)" =
    let program = {|
print([1,2,3][2 - (2 / 2)])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 2 |}]

let%expect_test "Test list access (5)" =
    let program = {|
print([[1,2,3], [4,5,6]][1][2 - (2 / 2)])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 5 |}]

let%expect_test "Test len()" =
    let program = {|
print(len([1,2,4]))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 3 |}]

let%expect_test "Test len() (2)" =
    let program = {|
print(len([1,2,4] * 10))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 30 |}]

let%expect_test "Test len() (3)" =
    let program = {|
print(len("this string is long"))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 19 |}]

let%expect_test "Test len() (4)" =
    let program = {|
print(len(3.14159))
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      Exception at statement 0:
      TypeError: object of type '(FloatV 3.14159)' has no len()
      Error: Program failed.
      |}]

let%expect_test "Test list and dict access" =
    let program = {|
d = {}

d[1] = 4
d[2] = 5
d[3] = 6
d["test"] = 7

print(d)
print(1 in d)
print(6 in d)
print("test" in d)
print("test" in {"nope": 1})

l = [1, 2, 3, "wow"]
print("\n", l)
print(1 in l)
print(6 in l)
print(6 in [1, 2, 3, "wow"])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      {2 : 5, "test" : 7, 3 : 6, 1 : 4}
      True
      False
      True
      False

       [1, 2, 3, "wow"]
      True
      False
      False
      |}]

let%expect_test "Test list and dict access (2)" =
    let program = {|

d = {}

d[1] = 4
d[2] = 5
d[3] = 6
d["test"] = 7

print(d)
print(1 in d, 6 in d, "test" in d)
print("test" in {"nope": 1})

l = [1, 2, 3, "wow"]
print("\n", l)
print(1 in l, 2 in l, "wow" in l)
print("test" in ["nope", False])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      {2 : 5, "test" : 7, 3 : 6, 1 : 4}
      True False True
      False

       [1, 2, 3, "wow"]
      True True True
      False
      |}]

let%expect_test "Test list and dict access (3)" =
    let program = {|
d = {1: "t", "jesus": True, False: 3.14, 3.14: "pi", "pi": 3.14, 3: "three", []: {"dict inside dict": True}, None: None}

print(d[1], d["jesus"], d[False], d[3.14], d["pi"], d[3], d[[]], d[None])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| t True 3.14 pi 3.14 three {"dict inside dict" : True} None |}]

let%expect_test "Test list and dict access (4)" =
    let program = {|
l = [1, 2, "3", "4", True, False, ["wow", "cool"]]
print(l[0], l[4], l[-1], ["wow", "cool"] in l, 1 not in l)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {| 1 True ["wow", "cool"] True False |}]

let%expect_test "Test string access" =
    let program = {|
s = "wow hey cool"
for i in range(len(s)):
    print(i, s[i])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      0 w
      1 o
      2 w
      3
      4 h
      5 e
      6 y
      7
      8 c
      9 o
      10 o
      11 l
      |}]

let%expect_test "Test not" =
    let program = {|
print(not "wow")
print(not [])
print(not {})
print(not 0.0)
print(not 15)
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      False
      True
      True
      True
      False
      |}]

let%expect_test "Test list slicing — basic ranges" =
    let program = {|
l = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
print(l[2:5])
print(l[:4])
print(l[6:])
print(l[:])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      [2, 3, 4]
      [0, 1, 2, 3]
      [6, 7, 8, 9]
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      |}]

let%expect_test "Test list slicing — step and reverse" =
    let program = {|
l = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
print(l[::2])
print(l[1:8:2])
print(l[::-1])
print(l[8:2:-2])
print(l[::-3])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      [0, 2, 4, 6, 8]
      [1, 3, 5, 7]
      [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      [8, 6, 4]
      [9, 6, 3, 0]
      |}]

let%expect_test "Test list slicing — negative indices" =
    let program = {|
l = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
print(l[-3:])
print(l[:-3])
print(l[-5:-2])
print(l[-1:-6:-1])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      [7, 8, 9]
      [0, 1, 2, 3, 4, 5, 6]
      [5, 6, 7]
      [9, 8, 7, 6, 5]
      |}]

let%expect_test "Test list slicing — out-of-range and empty" =
    let program = {|
l = [1, 2, 3, 4, 5]
print(l[100:])
print(l[:100])
print(l[-100:2])
print(l[3:1])
print(l[1:1])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      []
      [1, 2, 3, 4, 5]
      [1, 2]
      []
      []
      |}]

let%expect_test "Test string slicing" =
    let program = {|
s = "hello world"
print(s[0:5])
print(s[6:])
print(s[:5])
print(s[::-1])
print(s[::2])
print(s[-5:])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      hello
      world
      hello
      dlrow olleh
      hlowrd
      world
      |}]

let%expect_test "Test slicing with computed indices" =
    let program = {|
l = [10, 20, 30, 40, 50, 60]
i = 1
j = 5
print(l[i:j])
print(l[i+1:j-1])
print(l[i:j:2])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      [20, 30, 40, 50]
      [30, 40]
      [20, 40]
      |}]

let%expect_test "Test slicing — zero step error" =
    let program = {|
l = [1, 2, 3]
print(l[::0])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      Exception at statement 1:
      ValueError: slice step cannot be zero
      Error: Program failed.
      |}]

let%expect_test "Test slicing — non-integer index error" =
    let program = {|
l = [1, 2, 3]
print(l["a":2])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      Exception at statement 1:
      TypeError: slice indices must be integers or None, not (StringV a)
      Error: Program failed.
      |}]

let%expect_test "Test slicing — chained access" =
    let program = {|
l = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
print(l[1:][0])
print(l[0][1:3])
|} in
    interpret ~file_name:program ~print_values:false ~load_stdlib:true ~interpret_string:true ();
    [%expect {|
      [5, 6, 7, 8]
      [2, 3]
      |}]


