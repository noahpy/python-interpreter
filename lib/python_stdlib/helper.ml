open Base
open Ast


module Helper_Checks = struct
    let check_arg_count (arguments: value list) (lower: int) (higher: int) (fun_name: string): (unit, string) Result.t= 
        let len = List.length arguments in
        if len >= lower && (len <= higher || higher = -1) then Ok() 
        else let msg = String.concat ~sep:" " ["TypeError:";fun_name;"takes from";(Int.to_string lower);"to";(Int.to_string higher);"positional arguments but";(Int.to_string len);"were given"]
             in Error(msg)

end

module Helper_Generator = struct

    let generate_fon_args (min_args: int) (max_args: int) (fun_name: string) : func_oncall  =
        (* Checks basic stuff and sets __args in state *)
        fun (args : value list) (state : program_state) -> 
            let c1 = Helper_Checks.check_arg_count args min_args max_args fun_name
            in match c1 with
              | Ok() -> Hashtbl.add_multi state.variables ~key:"__args" ~data:(Value(ListV(args))); Ok()
              | Error(x) -> Error(x)

    let generate_foff_args (): func_offcall = 
        (* Unset __args in state *)
        fun (state : program_state) -> 
            Hashtbl.remove state.variables "__args"; Ok()


end
