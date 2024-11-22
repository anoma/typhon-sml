structure HPaxosValue : HPAXOS_VALUE =
struct
    type t = word

    val default = Word.fromInt 0
    val eq : t * t -> bool = (op =)
end
