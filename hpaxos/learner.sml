signature LEARNER =
sig
    type t
    val id : t -> word
    val eq : t * t -> bool
end

functor LearnerOrdKey (L : LEARNER) : ORD_KEY =
struct
    type ord_key = L.t
    fun compare (l1, l2) = Word.compare (L.id l1, L.id l2)
end
