open Ast

let check_arg_count (arguments: value list) (lower: int) (higher: int) (fun_name: string): (unit, string) result = 
    let len = List.length arguments in
    if len >= lower && len <= higher then Ok() 
    else let msg = String.concat " " ["TypeError:";fun_name;"takes from";(string_of_int lower);"to";(string_of_int higher);"positional arguments but";(string_of_int len);"were given"]
         in Error(msg)
