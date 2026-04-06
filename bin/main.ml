
open Base
module Interpret = Python_interpreter.Interpreter
open Python_interpreter.Ast
open Python_interpreter.Utils


let () = 
    let f_on args state =
        Hash_utils.add_variable state "x" (Value(List.nth_exn args 0));
        Hash_utils.add_variable state "y" (Value(List.nth_exn args 1));
        Ok()
    in let f =
        Func_Stat [Return (Bin_Exp(Var_Ref("x"), Add, Var_Ref("y")))]
    in let f_off state =
        Hash_utils.remove_variable state "x";
        Hash_utils.remove_variable state "y";
        Ok()
    in let line1 = Assign("y", Value(IntV(1))) in
    let line2 = Assign("x", Value(IntV(2))) in
    let line3 = Func_Def("add", f, f_on, f_off)in
    let line4 = Assign("res", Func_App("add", [Var_Ref("y"); Var_Ref("x")])) in
    let line5 = Expr(Func_App("print", [Var_Ref("res"); Var_Ref("x"); Var_Ref("y")])) in
    let lines = [line1; line2; line3; line4; line5] in 
    let p = Interpret.init_program_state ~load_stdlib:true lines in
    Interpret.interpret_top p

