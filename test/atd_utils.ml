module Variant_from_type : sig
  val normalize : Yojson.Safe.t -> Yojson.Safe.t
  val restore : Yojson.Safe.t -> Yojson.Safe.t
end =
struct
  let normalize (j: Yojson.Safe.t) = match j with
    | `String s -> `List [ `String "Single"; `String s ]
    | `List l -> `List [ `String "Multiple"; `List l ]
    | a -> a

  let restore a = a
end
