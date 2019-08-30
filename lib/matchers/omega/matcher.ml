open Core
open Angstrom

open Types

let configuration_ref = ref (Configuration.create ())

let matches_ref : Match.t list ref = ref []

let source_ref : string ref = ref ""

let current_environment_ref : Match.Environment.t ref = ref (Match.Environment.create ())

let (|>>) p f =
  p >>= fun x -> return (f x)

let debug = false

let record_match_context pos_before pos_after =
  let open Match.Location in
  if debug then Format.printf "match context start pos: %d@." pos_before;
  if debug then Format.printf "match context end pos %d@." pos_after;
  (* FIXME this may be slow. Try (a) collecting this
     or (b) removing it by just doing a rewrite *)
  let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
    String.slice source match_start match_end
  in
  let match_context =
    let match_start =
      { default with offset = pos_before }
    in
    let match_end =
      { default with offset = pos_after }
    in
    Match.
      { range = { match_start; match_end }
      ; environment = !current_environment_ref
      ; matched = extract_matched_text !source_ref match_start match_end
      }
  in
  matches_ref := match_context :: !matches_ref


let r _v =
  let open Match in
  let open Range in
  match _v with
  | String s ->
    if debug then Format.printf "Result: %s@." s;
    return _v
  | Match (pos_after, identifier, content) ->
    (* using just pos after for now, because thats what we do in matcher. lol *)
    if debug then Format.printf "Match: %S@." content;
    if debug then Format.printf "Match @@ %d@." pos_after;
    let before = Location.default in (* FIXME *)
    let after =
      { Location.default with offset = pos_after }
    in
    let range = { match_start = before; match_end = after } in
    let environment = Environment.add ~range !current_environment_ref identifier content in
    current_environment_ref := environment;
    return _v
  | _ -> return _v

module Make (Syntax : Syntax.S) = struct

  let between left right p =
    left *> p <* right

  let until_of_from from =
    Syntax.user_defined_delimiters
    |> List.find_map ~f:(fun (from', until) -> if from = from' then Some until else None)
    |> function
    | Some until -> until
    | None -> assert false

  let alphanum =
    satisfy (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> true
        | _ -> false)

  let reserved_delimiters =
    List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
    |> List.append [":["; "]"] (* hole *)
    |> List.append [":[["; "]]"] (* single token hole *)
    |> List.map ~f:string
    |> choice

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let reserved =
    reserved_delimiters
    <|> (satisfy is_whitespace |>> Char.to_string)

  let reserved_delimiters =
    List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
    |> List.append [":["; "]"]
    |> List.append [":[["; "]]"]

  let reserved =
    reserved_delimiters @ [" "; "\n"; "\t"; "\r"]
    |> List.sort ~compare:(fun v2 v1 ->
        String.length v1 - String.length v2)

  (* XXX can shortcircuit *)
  (* what if you hit a reserved
     seqence "{" and then attempt
     ":[[" and then say "end of
     input" and then move ahead any_char. not good.
     going from longest to shortest works though *)
  let any_char_except ~reserved =
    List.fold reserved
      ~init:(return `OK)
      ~f:(fun acc reserved_sequence ->
          option `End_of_input
            (peek_string (String.length reserved_sequence)
             >>= fun s ->
             if s = reserved_sequence then
               return `Reserved_sequence
             else
               acc))
    >>= function
    | `OK ->
      any_char
    | `End_of_input ->
      any_char
    | `Reserved_sequence -> fail "reserved sequence hit"

  let sequence_chain_deprecated p_list =
    List.fold p_list ~init:(return (String "")) ~f:(fun acc p ->
        acc >>= fun acc ->
        p >>= fun s ->
        match acc, s with
        | String acc, String s -> return (String (acc^s))
        | String acc, Unit -> return (String acc)
        | _ -> assert false)

  let generate_single_hole_parser () =
    (alphanum <|> char '_') |>> String.of_char

  let generate_greedy_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter
      () =
    let between_nested_delims p from =
      let until = until_of_from from in
      between (string from) (string until) p
      >>= fun result -> return (String.concat @@ [from] @ result @ [until])
    in
    let between_nested_delims p =
      (match left_delimiter, right_delimiter with
       | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
       | _ -> Syntax.user_defined_delimiters)
      |> List.map ~f:fst
      |> List.map ~f:(between_nested_delims p)
      |> choice
    in
    let reserved =
      (match left_delimiter, right_delimiter with
       | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
       | _ -> Syntax.user_defined_delimiters)
      |> List.concat_map ~f:(fun (from, until) -> [from; until])
    in
    fix (fun grammar ->
        let delimsx = between_nested_delims (many grammar) in
        let other = any_char_except ~reserved |>> String.of_char in
        (* FIXME comment, literals here *)
        choice
          [ delimsx
          ; other
          ])

  let cons x xs = x :: xs

  let many_till p t =
    fix (fun m ->
        (t *> return []) <|> (lift2 cons p m))

  let many1_till p t =
    lift2 cons p (many_till p t)

  (* when we have a nest, just insert a return Unit as the end (init) *)
  let sequence_chain p_list =
    (*Format.printf "Sequence chain %d@." @@ List.length p_list;*)
    (*Format.printf "ponk, list running sz: %d@." @@ List.length p_list;*)
    (* this case means that, if there are only holes, that empty string can sat
       (since we added that explicitly). but (many empty) is not good, so it's
       the edge case. adding () around template and source hack essentially
       removes the (many empty) case *)
    let only_holes =
      List.map p_list ~f:(fun p ->
          match parse_string p "_signal_hole" with
          | Error _ -> false
          | Ok _ -> true)
      |> List.for_all ~f:ident
    in
    if only_holes then
      begin
        if debug then Format.printf "ONLY HOLES: P SIZE: %d@." @@ List.length p_list;
        (many1_till
           (
             (*Format.printf "before running gen@.";*)
             generate_greedy_hole_parser ()
             >>= fun s ->
             (*Format.printf "after running gen: %S@." s;*)
             pos >>= fun pos ->
             if debug then Format.printf "captured pos: %d@." pos;
             return (s,pos)
             (* capture greedy pos in here before acc *)
           )
           end_of_input)
        >>= fun results ->
        (*Format.printf "have results %d@." @@ List.length results;*)
        r (Match (List.map results ~f:snd |> List.rev |> List.hd_exn, "1",
                  (String.concat (List.map results ~f:fst))))
      end
    else
      begin
        if debug then Format.printf "not just holes@.";
        (* If terminator size is 0, then do something else (terminate at eof?) *)
        List.fold_right p_list ~init:(return Unit) ~f:(fun p acc ->
            if debug then Format.printf "iterate@.";
            match parse_string p "_signal_hole" with
            | Error _ ->
              if debug then Format.printf "Composing p with terminating parser@.";
              p *> acc
            | Ok f ->
              (*Format.printf "Ok.@.";*)
              match f with
              | Hole (Alphanum (identifier, _)) ->
                pos >>= fun pos_before ->
                many1 (generate_single_hole_parser ())
                >>= fun value -> r (Match (pos_before, identifier, (String.concat value)))
              | Hole (Everything (identifier, _dimension)) ->
                if debug then Format.printf "do hole@.";
                (*Format.printf "match@.";*)
                (*pos >>= fun _pos_before ->*) (* why does this cause below to print twice ? *)
                (*Format.printf "hole detected@.";*)
                (* absolutely must use many_till because it stops the greedy
                   from eating the suffix.
                   greedy followed by suffix is not the same as greedy until suffix:
                   greedy will eat the suffix and then expect the suffix too, and fail *)

                (choice [
                    (many1_till
                       (
                         (*Format.printf "before running gen@.";*)
                         generate_greedy_hole_parser ()
                         >>= fun s ->
                         (*Format.printf "after running gen: %S@." s;*)
                         pos >>= fun pos ->
                         if debug then Format.printf "captured pos: %d@." pos;
                         return (s,pos)
                         (* capture greedy pos in here before acc *)
                       )
                       acc)
                    ;
                    (* unifies hole with empty string if acc succeeds and above fails.
                       Cases like: (:[1]), (:[1]:[2]) with ().
                       works as expecred, EXCEPT when there is just an empty hole.
                       Because acc is the empty string... *)
                    (acc >>= fun _b ->
                     if debug then Format.printf "Acc succeeds.@.";
                     pos >>= fun pos ->
                     if debug then Format.printf "Pos %d@." pos;
                     at_end_of_input >>= fun res ->
                     if debug then Format.printf "End? %b@." res;
                     (*if res then
                       fail "x"
                       else*)
                     return ["", pos]
                    )
                  ]
                )
                >>= fun results ->
                (*Format.printf "have results %d@." @@ List.length results;*)
                r (Match (List.map results ~f:snd |> List.rev |> List.hd_exn, identifier,
                          (String.concat (List.map results ~f:fst))))
              | _ -> assert false)
      end

  (** must have at least one, otherwise spins on
      the empty string *)
  let spaces1 =
    satisfy is_whitespace *>
    (* XXX use skip_while once everything works.
       we don't need the string *)
    take_while is_whitespace

  (* XXX change ignore to unit once everything works.
     right now it's the string that was parsed by spaces1 *)
  let generate_spaces_parser _ignored =
    (* FIXME needs comments, etc. *)
    spaces1
    >>= fun result -> r (String result)

  let generate_string_token_parser str =
    (* FIXME needs comments, etc*)
    if debug then Format.printf "String(%s)@." str;
    string str
    >>= fun result -> r (String result)

  let generate_single_hole_parser () =
    (alphanum <|> char '_') |>> String.of_char

  let skip_any p =
    p |>> ignore

  let single_hole_parser () =
    string ":[["
    *> (many (alphanum <|> char '_')
        |>> String.of_char_list) <* string "]]"

  let greedy_hole_parser () =
    string ":["
    *> (many (alphanum <|> char '_')
        |>> String.of_char_list) <* string "]"

  let many1_till p t =
    let cons x xs = x::xs in
    lift2 cons p (many_till p t)

  let hole_parser sort dimension =
    match sort with
    | `Single ->
      single_hole_parser () |>> fun id ->
      skip_any (string "_signal_hole")
      |>> fun () -> Hole (Alphanum (id, dimension))
    | `Greedy ->
      greedy_hole_parser () |>> fun id ->
      skip_any (string "_signal_hole")
      |>> fun () -> Hole (Everything (id, dimension))

  let general_parser_generator : production t t =
    fix (fun (_generator : production t list t) ->
        if debug then Format.printf "Descends@.";
        let _nested =
          if debug then Format.printf "Nested@.";
          Syntax.user_defined_delimiters
          |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
              (string left_delimiter
               *> _generator
               <* string right_delimiter)
              >>= fun (g: production t list) ->
              if debug then Format.printf "G size: %d; delim %s@." (List.length g) left_delimiter;
              (([string left_delimiter
                 >>= fun result -> r (String result)]
                @ g
                @ [ string right_delimiter
                    >>= fun result -> r (String result)])
               |>
               sequence_chain)
              |> return)
          |> choice
        in
        let _spaces = spaces1 |>> generate_spaces_parser in
        let _other =
          (* XXX many1_till would be cool, but it also parses the thing that
             causes it to fail, which i need restored.  many_till is 'parse and
             include the parse of the exception', whereas I want parse and
             exclude the parse of the exception (hard to reintroduce ) *)
(*
             (many1_till (any_char >>= fun c -> Format.printf "parsed %c@." c;
             return c) (List.map reserved ~f:string |> choice >>= fun x ->
             Format.printf "Fail on %s@." x; return x) |>> fun s ->
             Format.printf "Chars: %s@." @@ String.of_char_list s;
             String.of_char_list s) *)
          (many1 (any_char_except ~reserved) |>> String.of_char_list)
          |>> fun x ->
          if debug then Format.printf "Other: %s@." x;
          generate_string_token_parser x in
        if debug then Format.printf "Many... @.";
        (* can't be many1 because then {} isn't a valid template (delimiters have to
           contain something then and can't be empty *)
        (* don't want it to be many because empty string will satisfy and
           "" is a valid template, or even "{", because it generates 'seq' on chain *)
        many (choice
                [ hole_parser `Single Code
                ; hole_parser `Greedy Code
                ; _spaces
                ; _nested
                ; _other
                ])
        >>= fun x ->
        if debug then Format.printf "Produced %d@." @@ List.length x;
        return x
      )
    |>> fun p_list ->
    p_list
    |> sequence_chain |> fun matcher ->
    (* XXX: skip_any needs to be delim/string literals *)
    many ((skip_any
             (many_till any_char
                (
                  (
                    (
                      at_end_of_input >>= fun res ->
                      if debug then Format.printf "End? %b. Then end.@." res;
                      if res then
                        fail "x"
                      else
                        pos >>= fun start_pos ->
                        matcher >>= fun _access_last_production_herpe ->
                        pos >>= fun end_pos ->
                        if debug then Format.printf "Fuckery@.";
                        record_match_context start_pos end_pos;
                        current_environment_ref := Match.Environment.create ();
                        return Unit)
                    (*<|>
                      (end_of_input >>= fun () -> return Unit)*)
                  )
                )
             )
          ))
    (*>>= fun _x -> end_of_input *)
    >>= fun _x -> r Unit >>= fun _x -> r Unit

  (* XXX could maybe use "end_of_input" parser as a better way of doing this *)
  let to_template template =
    let state = Buffered.parse general_parser_generator in
    Buffered.feed state (`String template)
    |> function
    | Buffered.Partial f ->
      let fin = f `Eof in
      begin
        match fin with
        | Buffered.Partial _ ->
          if debug then Format.printf "(1) template FAIL. not fully consumed@.";
          Or_error.error_string "(1) template not fully consumed@.";
        | Buffered.Done ({ len; _ },p) ->
          if len = 0 then
            (if debug then Format.printf "Template fully consumed@.";
             Ok p)
          else
            (if debug then Format.printf "(X)template FAIL. not fully consumed";
             Or_error.error_string "(X)template not fully consumed")
        | Buffered.Fail (_,_,msg) ->
          if debug then Format.printf "template NOT ok@.";
          Or_error.error_string msg
      end
    | Buffered.Done ({ len; _ },p) ->
      if len = 0 then
        (if debug then Format.printf "Template fully consumed@.";
         Ok p)
      else
        (if debug then Format.printf "(2)template FAIL. not fully consumed@.";
         Or_error.error_string "(2)template not fully consumed.")
    | Buffered.Fail (_,_,msg) ->
      if debug then Format.printf "template NOT ok@.";
      Or_error.error_string msg

  let first' p source
    : Match.t Or_error.t
    =
    source_ref := source;
    let state = Buffered.parse p in
    let state = Buffered.feed state (`String source) in
    let state = Buffered.feed state `Eof in
    match state with
    | Buffered.Done ({ len; buf = _; _ }, _result) ->
      if debug then Format.printf "Buffered Done. Unconsumed: %d@." len;
      Ok (Match.create ())
    | Buffered.Fail (_, _, msg) ->
      Format.printf "fail, parse NOT ok :%s@." msg;
      Or_error.error_string msg
    | _ ->
      Format.printf "partial, parse NOT ok@.";
      Or_error.error_string "partial, parse NOT ok"

  let first ?configuration:_ ?shift:_ template source : Match.t Or_error.t =
    match to_template template with
    | Ok p ->
      first' p source |> ignore;
      begin
        match !matches_ref with
        | [] -> Or_error.error_string "not really"
        | hd::_ -> Ok hd
      end
    | Error e ->
      Format.printf "Template FAIL %s@." @@ Error.to_string_hum e;
      Error e

  let all ?configuration:_ ~template ~source : Match.t list =
    match first template source with
    | Ok _
    | Error _ -> !matches_ref
end
