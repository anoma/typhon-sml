signature ACCEPTOR =
sig
    type t

    val pubkey : t -> word
    val eq : t * t -> bool
end
