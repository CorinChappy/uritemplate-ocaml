type value_modifier =
  | NoModifier
  | Prefix of int (** \{var:N\} *)
  | Composite (** \{var*\} *)
(** A modifier for a variable *)

type variable_expression
(** A variable in an expression, optionally containing a prefix or composite modifier *)

type expression
(** A URI template expression. This can be expanded into a real string when the URI is templated *)

type template_part =
  | Literal of string
  | Expression of expression

(** A part of a template *)

type t
(** The type of a template *)

val empty : t

val add_part : template_part -> t -> t

val add_literal : string -> t -> t
val add_expression : Expansion_type.t -> variable_expression list -> t -> t
val add_single_expression : Expansion_type.t -> variable_expression -> t -> t

val create : template_part list -> t

val create_expression : Expansion_type.t -> variable_expression list -> expression

val create_single_expression : Expansion_type.t -> variable_expression -> expression

val create_variable_expression : ?value_modifier:value_modifier -> string -> variable_expression


val parts_of_t : t -> template_part list

val get_expansion_type : expression -> Expansion_type.t
val get_variable_expressions : expression -> variable_expression list

val get_variable_expression_name : variable_expression -> string
val get_variable_expression_modifier : variable_expression -> value_modifier

val part_is_literal : template_part -> bool
val part_is_expression : template_part -> bool

val get_variable_names : t -> string list
