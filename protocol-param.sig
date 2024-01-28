signature PROTOCOL_PARAM =
sig
    type t
    (* TODO check if used *)
    val id : t -> int
    val toString : t -> string
end
