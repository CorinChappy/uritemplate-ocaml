type value_modifier =
  | NoModifier
  | Prefix of int (* {var:N} *)
  | Composite (* {var*} *)

type variable_expression = {
  name: string;
  value_modifier: value_modifier;
}

type expression = {
  expansion_type: Expansion_type.t;
  variable_expressions: variable_expression list;
}

type template_part =
  | Literal of string
  | Expression of expression

type t = {
  parts: template_part list
}


let create parts = { parts }

let create_expression expansion_type variable_expressions = {
  expansion_type;
  variable_expressions
}

let create_single_expression expansion_type variable_expression = {
  expansion_type;
  variable_expressions = [variable_expression]
}

let create_variable_expression ?value_modifier:(value_modifier = NoModifier) name = {
  name;
  value_modifier
}


let parts_of_t { parts; } = parts

let get_expansion_type { expansion_type; _ } = expansion_type
let get_variable_expressions { variable_expressions; _ } = variable_expressions

let get_variable_expression_name { name; _ } = name
let get_variable_expression_modifier { value_modifier; _ } = value_modifier
