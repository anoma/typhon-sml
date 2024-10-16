structure HPaxosAcceptor =
struct
    type t = word

    fun pubkey a = a
    val eq : t * t -> bool = (op =)
end

functor AcceptorOrdKey (A : ACCEPTOR) : ORD_KEY =
struct
    type ord_key = A.t
    fun compare (a1, a2) = Word.compare (A.pubkey a1, A.pubkey a2)
end
