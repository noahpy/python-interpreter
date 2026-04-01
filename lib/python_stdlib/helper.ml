open Base
open Ast

let check_arg_count (arguments: value list) (lower: int) (higher: int) (fun_name: string): (unit, string) Result.t= 
    let len = List.length arguments in
    if len >= lower && len <= higher then Ok() 
    else let msg = String.concat ["TypeError:";fun_name;"takes from";(Int.to_string lower);"to";(Int.to_string higher);"positional arguments but";(Int.to_string len);"were given"]
         in Error(msg)
