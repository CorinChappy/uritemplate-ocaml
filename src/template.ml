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


let create parts = { parts = List.rev parts }

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

let empty = { parts = [] }

let add_part t part = { parts = part::t.parts }

let add_literal t lit = add_part t (Literal lit)

let add_expression t expansion_type variable_expressions =
  Expression (create_expression expansion_type variable_expressions)
  |> add_part t

let add_single_expression t expansion_type variable_expression =
  Expression (create_single_expression expansion_type variable_expression)
  |> add_part t


let parts_of_t { parts; } = List.rev parts

let get_expansion_type { expansion_type; _ } = expansion_type
let get_variable_expressions { variable_expressions; _ } = variable_expressions

let get_variable_expression_name { name; _ } = name
let get_variable_expression_modifier { value_modifier; _ } = value_modifier

let part_is_literal = function
  | Literal _ -> true
  | _ -> false
let part_is_expression = function
  | Expression _ -> true
  | _ -> false

let get_variable_names { parts; } =
  List.filter part_is_expression parts
  |> List.map (function
      | Expression e -> (
          get_variable_expressions e
          |> List.map get_variable_expression_name
        )
      | _ -> assert false
    )
  |> List.rev
  |> List.flatten
