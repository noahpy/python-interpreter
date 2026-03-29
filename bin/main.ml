
module Interpret = Python_interpreter.Interpreter
module Eval = Python_interpreter.Eval.Eval_ex
open Python_interpreter.Ast


let () = 
    let line1 = Assign("x", Bin_Exp(Value(FloatV 10.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line2 = Assign("y", Bin_Exp(Value(FloatV 12.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line3 = Expr(Bin_Exp(Var_Ref("x"), Add, Var_Ref("y"))) in
    let lines = [line1; line2; line3] in 
    let varH = Hashtbl.create 10 in
    let p = {program = lines; ip = 0; variables = varH;} in
    Interpret.interpret p

