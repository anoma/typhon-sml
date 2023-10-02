signature ACCEPTOR =
sig
    type t
    val pubkey : t -> word
end

functor AcceptorOrdKey (A : ACCEPTOR) : ORD_KEY =
struct
    type ord_key = A.t
    fun compare (a1, a2) = Word.compare (A.pubkey a1, A.pubkey a2)
end
