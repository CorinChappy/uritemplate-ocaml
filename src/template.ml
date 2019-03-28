type expansion_type =
  | Simple (* {var} *)
  | Reserved (* {+var} *)
  | Fragment (* {#var} *)
  | Dot (* {.var} *)
  | PathSegment (* {/var} *)
  | PathParameter (* {;var} *)
  | FormQuery (* {?var} *)
  | FormQueryContinuation (* {&var} *)

type value_modifier =
  | NoModifier
  | Prefix of int (* {var:N} *)
  | Composite (* {var*} *)

type variable_expression = {
  name: string;
  value_modifier: value_modifier;
}

type expression = {
  expansion_type: expansion_type;
  variable_expressions: variable_expression list;
}

type t_part =
  | Literal of string
  | Expression of expression

type t = {
  template: t_part list
}


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


let create_variable_expression name value_modifier = {
  name; value_modifier;
}

let create_expression expansion_type variable_expressions = {
  expansion_type; variable_expressions;
}

let is_var_expr v = Str.string_match Regex.for_is_var_expr v 0


let variable_expression_of_string str =
  (* Detect a composite value *)
  match Str.string_match Regex.compsite_from_var_name str 0 with
  | true -> (
      let new_name = Str.matched_group 1 str in
      create_variable_expression new_name Composite
    )
  | false -> (
      match Str.string_match Regex.trim_from_var_name str 0 with
      (* -1 will cause sub to raise Invalid_argument, and return the full var *)
      | false -> create_variable_expression str NoModifier
      | true -> (
          let new_name = Str.matched_group 1 str in
          let trim_num = Str.matched_group 2 str |> int_of_string in
          create_variable_expression new_name (Prefix trim_num)
        )
    )

let part_of_string str =
  match is_var_expr str with
  | false -> Literal str
  | true ->
    let _ = Str.string_match Regex.for_prefix str 0 in
    let expansion_type = Str.matched_group 1 str |> expansion_type_of_string in
    let expression_strings = Str.matched_group 2 str |> String.split_on_char ',' in
    let variable_expressions = List.map variable_expression_of_string expression_strings in
    Expression (create_expression  expansion_type variable_expressions)


let parts_of_string str =
  let rec aux index parts =
    if Str.string_match Regex.for_tokens str index && index < (String.length str) then
      let new_index = Str.match_end () in
      let part = Str.matched_string str |> part_of_string in
      aux new_index (part::parts)
    else
      List.rev parts
  in
  aux 0 []

let of_string str =
  let template = parts_of_string str in
  { template; }
