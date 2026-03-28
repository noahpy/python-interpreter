
module Interpret = Python_interpreter.Interpreter
module Eval = Python_interpreter.Eval.Eval_ex
open Python_interpreter.Ast
open Python_interpreter.Program_state


let () = 
    let line1 = Assign("x", Bin_Exp(Value(FloatV 10.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line2 = Assign("x", Bin_Exp(Value(FloatV 12.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line3 = Expr(Bin_Exp(Variable("x"), Add, Value(IntV(4)))) in
    let lines = [line1; line2; line3] in 
    let varH = Hashtbl.create 10 in
    let funH = Hashtbl.create 10 in
    let p = {program = lines; ip = 0; variables = varH; functions = funH} in
    Interpret.interpret p

