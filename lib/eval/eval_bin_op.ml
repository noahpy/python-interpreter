
open Ast

(* Although implicit casting varies from operand to operand in Python,
   we generalize it here for simplicity. *)
let implicit_casting (val1: value) (val2: value) : (value * value) =
    match (val1 , val2) with
      | (IntV x1, IntV x2) -> (IntV x1, IntV x2)
      | (IntV x1, FloatV x2) -> (FloatV (float_of_int x1), FloatV x2)
      | (FloatV x1, IntV x2) -> (FloatV x1, FloatV (float_of_int x2))
      | (FloatV x1, FloatV x2) -> (FloatV x1, FloatV x2)
      | (IntV x1, StringV x2) -> (StringV (string_of_int x1), StringV x2)
      | (StringV x1, IntV x2) -> (StringV x1, StringV (string_of_int x2))
      | (FloatV x1, StringV x2) -> (StringV (string_of_float x1), StringV x2)
      | (StringV x1, FloatV x2) -> (StringV x1, StringV (string_of_float x2))
      | (StringV x1, StringV x2) -> (StringV x1, StringV x2)
      | (BoolV x1, IntV x2) -> (IntV (if x1 then 1 else 0), IntV x2)
      | (IntV x1, BoolV x2) -> (IntV x1, IntV (if x2 then 1 else 0))
      | (BoolV x1, FloatV x2) -> (FloatV (if x1 then 1.0 else 0.0), FloatV x2)
      | (FloatV x1, BoolV x2) -> (FloatV x1, FloatV (if x2 then 1.0 else 0.0))
      | (BoolV x1, StringV x2) -> (StringV (string_of_bool x1), StringV x2)
      | (StringV x1, BoolV x2) -> (StringV x1, StringV (string_of_bool x2))
      | (BoolV x1, BoolV x2) -> (BoolV x1, BoolV x2)
      (* Pass on exceptions *)
      | (Exception e1, x) -> (Exception e1, x)
      | (x, Exception e2) -> (x, Exception e2)
      (* Match all rest cases including Ntwo and Exception *)
      | _ -> (Exception "We do not support this implicit type conversion.", Exception "")


(* Evaluation functions for each value type *)
let eval_int_op (x1: int) (op: bin_op) (x2: int) : value = 
    match op with
      | Add -> IntV(x1 + x2)
      | Mul -> IntV(x1 * x2)
      | Sub -> IntV(x1 - x2)
      | Div -> if x2 = 0 then Exception "ZeroDivisionError: division by zero" else IntV(x1 / x2)
      | Less -> BoolV(x1 < x2)
      | Greater -> BoolV(x1 > x2)
      | Leq -> BoolV(x1 <= x2)
      | Geq -> BoolV(x1 >= x2)
      | Equal -> BoolV(x1 = x2)
      | Neq -> BoolV(x1 <> x2)
      | And -> IntV(max x1 x2)
      | Or -> IntV(min x1 x2)


let eval_float_op (x1: float) (op: bin_op) (x2: float) : value = 
    match op with
      | Add -> FloatV(x1 +. x2)
      | Mul -> FloatV(x1 *. x2)
      | Sub -> FloatV(x1 -. x2)
      | Div -> if x2 = 0.0 then Exception "ZeroDivisionError: float division by zero" else FloatV(x1 /. x2)
      | Less -> BoolV(x1 < x2)
      | Greater -> BoolV(x1 > x2)
      | Leq -> BoolV(x1 <= x2)
      | Geq -> BoolV(x1 >= x2)
      | Equal -> BoolV(x1 = x2)
      | Neq -> BoolV(x1 <> x2)
      | And -> FloatV(max x1 x2)
      | Or -> FloatV(min x1 x2)


let eval_string_op (x1: string) (op: bin_op) (x2: string) : value = 
    match op with
      | Add -> StringV(String.concat "" [x1; x2])
      | Less -> BoolV(x1 < x2)
      | Greater -> BoolV(x1 > x2)
      | Leq -> BoolV(x1 <= x2)
      | Geq -> BoolV(x1 >= x2)
      | Equal -> BoolV(x1 = x2)
      | Neq -> BoolV(x1 <> x2)
      | _ -> Exception "Operation not supported for type string"


let eval_bool_op (x1: bool) (op: bin_op) (x2: bool) : value = 
    match op with
      | And -> BoolV(x1 && x2)
      | Or -> BoolV(x1 || x2)
      | Equal -> BoolV(x1 = x2)
      | Neq -> BoolV(x1 <> x2)
      | _ -> Exception "Operation not supported for type bool"
