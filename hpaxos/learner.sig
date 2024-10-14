signature LEARNER =
sig
    type t
    val id : t -> word
    val hash : t -> word

    val eq : t * t -> bool
    val gt : t * t -> bool
    val compare : t * t -> order
end
