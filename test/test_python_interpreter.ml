open! Base
open Stdio

let%expect_test "Testing addition" = 
    let x = 5 in
    print_s [%sexp (x : int)];
  [%expect {| 5 |}]
