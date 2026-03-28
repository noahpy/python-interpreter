
open Ast
open Eval

let rec eval_expr ex =
    match ex with
      | Value val_x -> val_x
      | Bin_Exp (ex1, op, ex2) -> eval_bin_op ex1 op ex2
      | Print ex -> Eval_print.eval_print (eval_expr ex)
      | _ -> Exception "Not implemented yet."

and eval_bin_op ex1 op ex2 =
    match Eval_bin_op.ops_casting (eval_expr ex1) (eval_expr ex2) with
      | (IntV x1, IntV x2) -> Eval_bin_op.eval_int_op x1 op x2
      | (FloatV x1, FloatV x2) -> Eval_bin_op.eval_float_op x1 op x2
      | (StringV x1, StringV x2) -> Eval_bin_op.eval_string_op x1 op x2
      | (BoolV x1, BoolV x2) -> Eval_bin_op.eval_bool_op x1 op x2
      | _ -> Exception "Can not operate on these types: Implicit type conversion missing."



    
