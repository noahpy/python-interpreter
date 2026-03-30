
module Interpret = Python_interpreter.Interpreter
module Eval = Python_interpreter.Eval.Eval_ex
open Python_interpreter.Ast


let () = 
    let line1 = Assign("x", Bin_Exp(Value(FloatV 10.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line2 = Assign("y", Bin_Exp(Value(FloatV 12.0), Div, Bin_Exp(Value(IntV 2), Mul, Value(IntV 3)))) in
    let line3 = Assign("x", Bin_Exp(Var_Ref("x"), Add, Var_Ref("y"))) in
    let line4 = Expr(Func_App("print", [Value(IntV 10); Var_Ref("x")])) in
    let lines = [line1; line2; line3; line4] in 
    let p = Interpret.init_program_state lines 10 in
    Interpret.interpret p

