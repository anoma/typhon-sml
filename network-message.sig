signature NETWORK_MESSAGE =
sig
    type t
    val hash : t -> word
    val eq : t * t -> bool
    val print : t -> string
    val fromString : string -> t
end
