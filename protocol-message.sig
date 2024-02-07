signature PROTOCOL_MESSAGE =
sig
    type t
    val sender : t -> int
    val hash : t -> word
    val eq : t * t -> bool
    val toString : t -> string
    val fromString : string -> t
end
