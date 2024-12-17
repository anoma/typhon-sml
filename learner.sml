signature LEARNER =
  sig
    type t
    val id : t -> word
    val eq : t * t -> bool
  end

functor LearnerOrdKey (L : LEARNER) : ORD_KEY =
  struct
    type ord_key = L.t
    fun compare (l1: ord_key, l2: ord_key): Int.int =
      case Word.compare (L.id l1, L.id l2) of
          LESS => ~1
        | EQUAL => 0
        | GREATER => 1
  end

