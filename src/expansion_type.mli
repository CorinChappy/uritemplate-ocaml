type t =
  | Simple (** \{var\} *)
  | Reserved (** \{+var\} *)
  | Fragment (** \{#var\} *)
  | Dot (** \{.var\} *)
  | PathSegment (** \{/var\} *)
  | PathParameter (** \{;var\} *)
  | FormQuery (** \{?var\} *)
  | FormQueryContinuation (** \{&var\} *)

val expansion_type_of_string : string -> t
val string_of_expansion_type : t -> string
val separator_for_expansion_type : t -> char
