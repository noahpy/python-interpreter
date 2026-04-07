
open Base
open Stdio
open Python_interpreter.Ast
open Python_interpreter.Utils
module Interpret = Python_interpreter.Interpreter
module Parser = Python_interpreter.Parser
module Lexer = Python_interpreter.Lexer



let () =
    let argv = Sys.get_argv() in 
    let filename = if Array.length argv > 1 then argv.(1) else "" in 
    Interpret.interpret ~file_name:filename ~print_values:false ~load_stdlib:true ~print_ir:false ()
