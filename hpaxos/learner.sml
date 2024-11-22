structure Learner : LEARNER =
struct
    type t  = word
    fun id a = a
    val eq : t * t -> bool = (op =)
    fun hash a = a

    fun compare (a, b) = Word.compare (id a, id b)
    fun gt (a, b) = case compare (a, b) of GREATER => true | _ => false
end

functor LearnerOrdKey (L : LEARNER) : ORD_KEY =
struct
    type ord_key = L.t
    fun compare (a, b) = L.compare (a, b)
end
