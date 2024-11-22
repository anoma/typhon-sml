structure HPaxosBallot : HPAXOS_BALLOT =
struct
    type t = word

    structure Value = HPaxosValue
    type value = Value.t

    val default = Word.fromInt 0

    val eq : t * t -> bool = (op =)
    val compare = Word.compare

    (* TODO *)
    fun hash b = b

    (* TODO *)
    fun value b = Value.default
end
