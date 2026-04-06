
let load_impls (state: Ast.program_state) : unit = 
    (* Load standard library functions. *)
    Io.load_impls state;

