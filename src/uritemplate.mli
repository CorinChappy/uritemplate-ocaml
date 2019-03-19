type variable = [
  | `String of string
  | `List of string list
]
(** The type of a variable that can be templated *)

val template_uri: template:string -> variables:(string * variable) list -> string
(** Templates the given string using the provided variables.
    {e Currently only compliant to level 3.} See {{:https://tools.ietf.org/html/rfc6570}RFC6570} *)

val template_uri_with_strings: template:string -> variables:(string * string) list -> string
(** A shorthand for template_uri when all variables are single strings *)

val template_uri_with_lists: template:string -> variables:(string * string list) list -> string
(** A shorthand for template_uri when all variables are lists of strings *)
