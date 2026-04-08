
open Base
module Interpreter = Python_interpreter.Interpreter

let () =
    let argv = Sys.get_argv() in 
    let filename = if Array.length argv > 1 then argv.(1) else "" in 
    Interpreter.interpret ~file_name:filename ~print_values:false ~load_stdlib:true ~print_ir:false ()
