
open Python_interpreter.Interpreter
open Python_interpreter.Ast

let () = 
    let ex = (Bin_Exp(Value(BoolV true), Greater, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let out = eval_expr ex in
    value_to_output out

