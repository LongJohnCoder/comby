open Core

open Matchers

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let%expect_test "dead_basic" =
  let source = {|shooop|} in
  let match_template = {|shooop|} in

  let match_ =
    Generic.first match_template source
    |> function
    | Ok ok -> ok
    | Error error ->
      failwith @@ Format.sprintf "->%s<-" (Error.to_string_hum error);
  in
  print_matches [match_];
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": -1, "column": -1 },
      "end": { "offset": 6, "line": -1, "column": -1 }
    },
    "environment": [],
    "matched": "shooop"
  }
]|}]

let%expect_test "dead_basic_with_whitespace" =
  let source = {|shoop da whoop|} in
  let match_template = {|shoop    da   whoop|} in

  let match_ =
    Generic.first match_template source
    |> function
    | Ok ok -> ok
    | Error _error ->
      failwith "x"
  in
  print_matches [match_];
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": -1, "column": -1 },
      "end": { "offset": 14, "line": -1, "column": -1 }
    },
    "environment": [],
    "matched": "shoop da whoop"
  }
]|}]

let%expect_test "test_reserved_delimiters" =
  let source = {|{}|} in
  let match_template = {|{}|} in

  let match_ =
    Generic.first match_template source
    |> function
    | Ok ok -> ok
    | Error error ->
      failwith @@ Format.sprintf "->%s<-" (Error.to_string_hum error);
  in
  print_matches [match_];
  [%expect_exact {|[
  {
    "range": {
      "start": { "offset": 0, "line": -1, "column": -1 },
      "end": { "offset": 2, "line": -1, "column": -1 }
    },
    "environment": [],
    "matched": "{}"
  }
]|}]

(*
let%expect_test "test_reserved_delimiters" =
  let source = {|({}([]))|} in
  let match_template = {|({}([]))|} in

  let match_ =
    Generic.first match_template source
    |> function
    | Ok ok -> ok
    | Error error ->
      failwith @@ Format.sprintf "->%s<-" (Error.to_string_hum error);
  in
  print_matches [match_];
  [%expect_exact {|Delimiter_{(Delimiter_{()Delimiter_{())Delimiter_((Delimiter_{()Delimiter_((Delimiter_[()))(matches (")"))
|}]
*)
