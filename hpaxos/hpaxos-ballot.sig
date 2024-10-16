signature HPAXOS_BALLOT =
sig
    type t
    type value

    val default : t

    val eq : t * t -> bool
    val compare : t * t -> order
    val hash : t -> word

    val value : t -> value
end
