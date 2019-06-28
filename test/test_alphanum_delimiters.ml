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
  [%expect_exact {|<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
body|}]

let%expect_test "nested_case_esac" =
  let source = {|case case body1 case body2 esac case body3 esac esac esac|} in
  let template = {|case :[1] esac|} in
  let rewrite_template = {|<block>:[1]</block>|} in
  Bash.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>1</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>2</c>
<sp>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>3</c>
<sp>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>1</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>2</c>
<sp>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>3</c>
<sp>
<sp>
<block>case body1 case body2 esac case body3 esac esac</block>|}]

let%expect_test "nested_case_esac_confirm_different_go" =
  let source = {|case case body1 case body2 esac case body3 esac esac esac|} in
  let template = {|case :[1] esac|} in
  let rewrite_template = {|<block>:[1]</block>|} in
  Go.all ~configuration ~template ~source
  |> Rewrite.all ~source ~rewrite_template
  |> (function
      | Some { rewritten_source; _ } -> print_string rewritten_source
      | None -> print_string "BROKEN EXPECT");
  [%expect_exact {|<c>c</c>
<c>a</c>
<c>s</c>
<c>e</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>1</c>
<sp>
<c>c</c>
<c>a</c>
<c>s</c>
<c>e</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>2</c>
<c>c</c>
<c>a</c>
<c>s</c>
<c>e</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>1</c>
<sp>
<c>c</c>
<c>a</c>
<c>s</c>
<c>e</c>
<sp>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>2</c>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>3</c>
<c>b</c>
<c>o</c>
<c>d</c>
<c>y</c>
<c>3</c>
<block>case body1 case body2</block> <block>body3</block> esac esac|}]

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
  [%expect_exact {|<c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>t</c>
<sp>
<c>=</c>
<sp>
<c>i</c>
<c>n</c>
<c>t</c>
<sp>
<c>m</c>
<c>o</c>
<c>d</c>
<c>u</c>
<c>l</c>
<c>e</c>
<sp>
<c>N</c>
<c>e</c>
<c>s</c>
<c>t</c>
<c>e</c>
<c>d</c>
<c>_</c>
<c>M</c>
<sp>
<c>=</c>
<sp>
<struct><c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>r</c>
<sp>
<c>=</c>
<sp>
<c>i</c>
<c>n</c>
<c>t</c>
<sp>
<end>type r = int
       <c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>t</c>
<sp>
<c>=</c>
<sp>
<c>i</c>
<c>n</c>
<c>t</c>
<sp>
<c>m</c>
<c>o</c>
<c>d</c>
<c>u</c>
<c>l</c>
<c>e</c>
<sp>
<c>N</c>
<c>e</c>
<c>s</c>
<c>t</c>
<c>e</c>
<c>d</c>
<c>_</c>
<c>M</c>
<sp>
<c>=</c>
<sp>
<struct><c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>r</c>
<sp>
<c>=</c>
<sp>
<c>i</c>
<c>n</c>
<c>t</c>
<sp>

    module M : sig
        type t
    end = struct <bye> end
<end>type r = int
       |}]

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
  let rewrite_template = {|<begin>:[1]<end>|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|<c>m</c>
<c>a</c>
<c>t</c>
<c>c</c>
<c>h</c>
<sp>
<c>x</c>
<sp>
<c>w</c>
<c>i</c>
<c>t</c>
<c>h</c>
<sp>
<c>|</c>
<sp>
<c>_</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>l</c>
<c>e</c>
<c>t</c>
<sp>
<c>m</c>
<c>o</c>
<c>d</c>
<c>u</c>
<c>l</c>
<c>e</c>
<sp>
<c>M</c>
<sp>
<c>=</c>
<sp>
<struct><c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>t</c>
<sp>
<end>type t <sp>
<begin><begin><c>m</c>
<c>a</c>
<c>t</c>
<c>c</c>
<c>h</c>
<sp>
<c>y</c>
<sp>
<c>w</c>
<c>i</c>
<c>t</c>
<c>h</c>
<sp>
<c>|</c>
<sp>
<c>_</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<sp>
<end>match y with
            | _ -> ()
            <sp>
<end>beginmatch y with
            | _ -> ()
            end
        <c>m</c>
<c>a</c>
<c>t</c>
<c>c</c>
<c>h</c>
<sp>
<c>x</c>
<sp>
<c>w</c>
<c>i</c>
<c>t</c>
<c>h</c>
<sp>
<c>|</c>
<sp>
<c>_</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>l</c>
<c>e</c>
<c>t</c>
<sp>
<c>m</c>
<c>o</c>
<c>d</c>
<c>u</c>
<c>l</c>
<c>e</c>
<sp>
<c>M</c>
<sp>
<c>=</c>
<sp>
<struct><c>t</c>
<c>y</c>
<c>p</c>
<c>e</c>
<sp>
<c>t</c>
<sp>
<end>type t <sp>
<begin><begin><c>m</c>
<c>a</c>
<c>t</c>
<c>c</c>
<c>h</c>
<sp>
<c>y</c>
<sp>
<c>w</c>
<c>i</c>
<c>t</c>
<c>h</c>
<sp>
<c>|</c>
<sp>
<c>_</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<sp>
<end>match y with
            | _ -> ()
            <sp>

    <begin>match x with
    | _ ->
        let module M = structtype t end
        beginbeginmatch y with
            | _ -> ()
            end
        end<end>
<end>beginmatch y with
            | _ -> ()
            end
        |}]

let%expect_test "erlang_blocks" =
  let source = {|Big =  fun(X) -> if X > 10 -> true; true -> false end end.|} in
  let match_template = {|fun(:[1]) :[rest] end|} in
  let rewrite_template = {|<fun>:[rest]<end>|} in

  run (module Matchers.Erlang) source match_template rewrite_template;
  [%expect_exact {|<c>X</c>
<c>-</c>
<c>></c>
<sp>
<if><c>X</c>
<sp>
<c>></c>
<sp>
<c>1</c>
<c>0</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>t</c>
<c>r</c>
<c>u</c>
<c>e</c>
<c>;</c>
<sp>
<c>t</c>
<c>r</c>
<c>u</c>
<c>e</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>f</c>
<c>a</c>
<c>l</c>
<c>s</c>
<c>e</c>
<sp>
<end>X > 10 -> true; true -> false <c>X</c>
<c>-</c>
<c>></c>
<sp>
<if><c>X</c>
<sp>
<c>></c>
<sp>
<c>1</c>
<c>0</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>t</c>
<c>r</c>
<c>u</c>
<c>e</c>
<c>;</c>
<sp>
<c>t</c>
<c>r</c>
<c>u</c>
<c>e</c>
<sp>
<c>-</c>
<c>></c>
<sp>
<c>f</c>
<c>a</c>
<c>l</c>
<c>s</c>
<c>e</c>
<sp>
Big =  <fun>-> ifX > 10 -> true; true -> false end<end>.<end>X > 10 -> true; true -> false |}]

let%expect_test "ruby_blocks" =
  let source = {| class before_ end |}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<class>:[1]<end>|} in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|<c>b</c>
<c>e</c>
No matches.|}]

(*
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
  let rewrite_template = {|<class>:[1]<end>|} in

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

let%expect_test "ruby_blocks_simpl" =
  let source = {|
class ActionController::Base
  classy
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
  classy
  before body
</Block>
|}]

let%expect_test "ruby_blocks_simpl" =
  let source = {|
class yclass
  before body
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = "<block>\n:[1]\n</Block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
<block>
yclass
  before body
</Block>
|}]

(* WRONG BEHAVIOR *)
let%expect_test "ruby_blocks_simpl" =
  let source = {|
class ActionController::Base
  classy
  before body
  endly
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = "<block>\n:[1]\n</Block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
<block>
ActionController::Base
  classy
  before body
</Block>ly
end
|}]

(* WRONG BEHAVIOR *)
let%expect_test "ruby_blocks_simpl" =
  let source = {|
class yclass
  before body
    endly
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = "<block>\n:[1]\n</Block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
<block>
yclass
  before body
</Block>ly
end
|}]


(* WRONG BEHAVIOR:
   ([x]) is matching ( with ].
   delim parser is not triggered. *)
let%expect_test "ruby_blocks_no_whitespace_before_delim" =
  let source = {| class class x end end |}
  in
  let match_template = {| class :[1] end |} in
  let rewrite_template = "<block>\n:[1]\n</block>" in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|<block>
class x
</block>end |}]
*)
