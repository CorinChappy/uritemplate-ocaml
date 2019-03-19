module String = Stdcompat.String

type expansion_type =
  | Simple (* {var} *)
  | Reserved (* {+var} *)
  | Fragment (* {#var} *)
  | Dot (* {.var} *)
  | PathSegment (* {/var} *)
  | PathParameter (* {;var} *)
  | FormQuery (* {?var} *)
  | FormQueryContinuation (* {&var} *)

let expansion_type_of_string = function
  | "+" -> Reserved
  | "#" -> Fragment
  | "." -> Dot
  | "/" -> PathSegment
  | ";" -> PathParameter
  | "?" -> FormQuery
  | "&" -> FormQueryContinuation
  | _ -> Simple

let string_of_expansion_type = function
  | Reserved -> "+"
  | Fragment -> "#"
  | Dot -> "."
  | PathSegment -> "/"
  | PathParameter -> ";"
  | FormQuery -> "?"
  | FormQueryContinuation -> "&"
  | Simple -> ""

let separator_for_expansion_type = function
  | Simple | Reserved | Fragment -> ','
  | Dot -> '.'
  | PathSegment -> '/'
  | PathParameter -> ';'
  | FormQuery | FormQueryContinuation -> '&'


type variable = [
  | `String of string
  | `List of string list
]

let is_var_expr v = Str.string_match Regex.for_is_var_expr v 0

(* Encoding *)
let encode_char c =
  Char.code c
  |> Printf.sprintf "%%%X"

let encode_str rex =
  Str.global_substitute
    rex
    (fun m -> String.get (Str.matched_string m) 0
              |> encode_char)

(* This is the same as a standard encodeUriComponent, but also encoding ! *)
let uri_encode_reserved = encode_str Regex.for_encode_reserved

let uri_encode_full = encode_str Regex.for_encode_full


let get_trim_from_var_name var_name =
  match Str.string_match Regex.trim_from_var_name var_name 0 with
  (* -1 will cause sub to raise Invalid_argument, and return the full var *)
  | false -> (var_name, -1)
  | true -> (
      let new_name = Str.matched_group 1 var_name in
      let trim_num = Str.matched_group 2 var_name |> int_of_string in
      (new_name, trim_num)
    )

let trim_var var trim =
  try
    String.sub var 0 trim
  with
  | Invalid_argument _ -> var

let trim_and_encode_var expr_type trim var =
  let trimmed_var = trim_var var trim in
  match expr_type with
  | Fragment
  | Reserved -> uri_encode_full trimmed_var
  | _ -> uri_encode_reserved trimmed_var


let simple_expr buff _ var =
  Buffer.add_string buff var

let form_query buff var_name var =
  Buffer.add_string buff var_name; Buffer.add_char buff '='; Buffer.add_string buff var

let path_parameter buff var_name var =
  Buffer.add_string buff var_name;
  match var with
  | "" -> ()
  | var -> Buffer.add_char buff '='; Buffer.add_string buff var

let determine_expr_function buff expr_type =
  buff
  |> match expr_type with
  | FormQuery | FormQueryContinuation -> form_query
  | PathParameter -> path_parameter
  | _ -> simple_expr

let rec combine_list_of_vars ?buff:(buff=Buffer.create 10) expr_type trim = function
  | [] -> ""
  | var::[] -> (
      trim_and_encode_var expr_type trim var |> Buffer.add_string buff;
      Buffer.contents buff
    )
  | var::rest -> (
      trim_and_encode_var expr_type trim var |> Buffer.add_string buff;
      Buffer.add_char buff ',';
      combine_list_of_vars ~buff expr_type trim rest
    )


let add_var_to_buff buff variables expr_type =
  let sep_str = separator_for_expansion_type expr_type in
  let f = determine_expr_function buff expr_type in
  fun var_name ->
    let (var_name, trim) = get_trim_from_var_name var_name in
    try
      let resolved_var = match List.assoc var_name variables with
        | `String var -> trim_and_encode_var expr_type trim var
        | `List vars -> combine_list_of_vars expr_type trim vars
      in
      f var_name resolved_var;
      Buffer.add_char buff sep_str
    with
    | Not_found -> ()

let create_buffer expr_type =
  let buff = Buffer.create 10 in
  match expr_type with
  | Reserved -> buff
  | _ -> string_of_expansion_type expr_type |> Buffer.add_string buff; buff


let replace_variable ~variables str =
  match is_var_expr str with
  | false -> uri_encode_full str
  | true ->
    let _ = Str.string_match Regex.for_prefix str 0 in
    let expansion_type = Str.matched_group 1 str |> expansion_type_of_string in
    let template_vars = Str.matched_group 2 str |> String.split_on_char ',' in
    let buff = create_buffer expansion_type in
    List.iter (add_var_to_buff buff variables expansion_type) template_vars;
    match Buffer.length buff - 1 with
    | -1 | 0 -> ""
    | len -> Buffer.sub buff 0 len


let template_uri ~template ~variables =
  let buff = Buffer.create 10 in
  let rec aux index =
    if Str.string_match Regex.for_tokens template index && index < (String.length template) then
      let new_index = Str.match_end () in
      let replaced_var = Str.matched_string template |> replace_variable ~variables in
      Buffer.add_string buff replaced_var;
      aux new_index
    else
      Buffer.contents buff
  in
  aux 0

let template_uri_with_strings ~template ~variables =
  template_uri
    ~template
    ~variables:(
      List.map
        (fun (var_name, var) -> (var_name, `String var))
        variables
    )