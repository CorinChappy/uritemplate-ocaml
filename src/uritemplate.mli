val template_uri: template:string -> variables:(string * string) list -> string
(** Templates the given string using the provided variables.
    {e Currently only compliant to level 3.} See {{:https://tools.ietf.org/html/rfc6570}RFC6570} *)
