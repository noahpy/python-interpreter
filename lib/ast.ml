
(* Abstract syntax tree of python subset *)

type value =
    | IntV of int
    | FloatV of float
    | StringV of string
    | BoolV of bool
    | Ntwo
    | Exception of string

type bin_op = 
    | Add
    | Mul
    | Sub
    | Div
    | And
    | Or
    | Less
    | Greater
    | Leq
    | Geq
    | Equal
    | Neq

type expr = 
    | Value of value
    | Bin_Exp of (expr * bin_op * expr)
    | Variable of string
    | Func_App of (string * expr list)


type statement = 
    | Expr of expr
    | Assign of (string * expr)
    | Func_Def of (string * expr)

type program = statement list

(* Helper functions *)

let value_to_output val_x =
    match val_x with
      | IntV x -> print_int x; print_newline();
      | FloatV x -> print_float x; print_newline();
      | StringV x -> print_string x; print_newline();
      | BoolV x -> print_string (if x then "True" else "False"); print_newline();
      | Exception x -> print_string x; print_newline();
      | Ntwo -> print_string "None"; print_newline();
