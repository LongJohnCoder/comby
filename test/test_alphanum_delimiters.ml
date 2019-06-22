open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()


let run ?(configuration = configuration) (module M : Matchers.Matcher) source match_template rewrite_template =
  M.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' ')) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let configuration = Configuration.create ~match_kind:Fuzzy ()

let all ?(configuration = configuration) template source =
  C.all ~configuration ~template ~source

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let%expect_test "base_case_esac" =
  let source = {|case body esac|} in
  let template = {|case :[1] esac|} in
  let rewrite_template = {|:[1]|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|body|}]

let%expect_test "nested_case_esac" =
  let source = {|case case body1 case body2 esac case body3 esac esac esac|} in
  let template = {|case :[1] esac|} in
  let rewrite_template = {|<block>:[1]</block>|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|<block>case body1 case body2 esac case body3 esac esac</block>|}]

let%expect_test "nested_case_esac_confirm_different_go" =
  let source = {|case case body1 case body2 esac case body3 esac esac esac|} in
  let template = {|case :[1] esac|} in
  let rewrite_template = {|<block>:[1]</block>|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|<block>case body1 case body2</block> <block>body3</block> esac esac|}]

let%expect_test "base_case_esac_no_whitespace_after" =
  let source = {|case body esac|} in
  let template = {|casecase :[1] esac|} in
  let rewrite_template = {|:[1]|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "NO MATCH EXPECTED");
  [%expect_exact {|NO MATCH EXPECTED|}]

let%expect_test "base_case_esac_no_whitespace_before" =
  let source = {|case body esac|} in
  let template = {|case :[1] esacesac|} in
  let rewrite_template = {|:[1]|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "NO MATCH EXPECTED");
  [%expect_exact {|NO MATCH EXPECTED|}]

let%expect_test "base_case_esac_partial_token" =
  let source = {|my mycase case body esac|} in
  let template = {|case :[1] esacesac|} in
  let rewrite_template = {|:[1]|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "NO MATCH EXPECTED");
  [%expect_exact {|NO MATCH EXPECTED|}]

(*
let%expect_test "ocaml_blocks" =
  let source = {|
    module M : sig
        type t
    end = struct
       type t = int

       module Nested_M = struct
         type r = int
       end
    end
|}
  in
  let match_template = {|struct :[1] end|} in
  let rewrite_template = {|struct <bye> end|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]
*)

let%expect_test "ocaml_complex_blocks_with_same_end" =
  let source = {|
    begin
    match x with
    | _ ->
        let module M = struct type t end
        begin
        begin
        match y with
        | _ -> ()
        end
        end
    end
|}
  in
  let match_template = {|begin :[1] end|} in
  let rewrite_template = {|-Body->:[1]<-Body-|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|
    -Body->match x with
    | _ ->
        let module M = struct type t<-Body-
        -Body->begin
        match y with
        | _ -> ()<-Body-
        end
    end
|}]

let%expect_test "erlang_blocks" =
  let source = {|Big =  fun(X) -> if X > 10 -> true; true -> false end end.|} in
  let match_template = {|fun(:[1]) :[rest] end|} in
  let rewrite_template = {|-Block->:[rest]<-Block-|} in

  run (module Matchers.Erlang) source match_template rewrite_template;
  [%expect_exact {|Big =  -Block->-> if X > 10 -> true; true -> false<-Block- end.|}]

let%expect_test "ruby_blocks" =
  let source = {|
class ActionController::Base
  before_filter :generate_css_from_less

  def generate_css_from_less
    Less::More.generate_all
  end
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|-Block->:[1]<-Block-|} in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
-Block->ActionController::Base
  before_filter :generate_css_from_less

  def generate_css_from_less
    Less::More.generate_all<-Block-
end
|}]

let%expect_test "ruby_blocks_simpl" =
  let source = {|
class ActionController::Base
  # don't interpret 'for' in before
  before body
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = "<block>\n:[1]\n</Block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
<block>
ActionController::Base
  # don't interpret 'for' in before
  before body
</Block>
|}]

let%expect_test "ruby_blocks_no_whitespace_before_delim" =
  let source = {| class class x end end |}
  in
  let match_template = {| class :[1] end |} in
  let rewrite_template = "<block>\n:[1]\n</block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|<block>
class x
</block>end |}]
