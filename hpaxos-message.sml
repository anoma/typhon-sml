(* HPaxos Message *)

signature HPAXOS_VALUE =
sig
    type t
end

signature HPAXOS_BALLOT =
sig
    type t
    val compare : t * t -> order
end

signature HPAXOS_MESSAGE =
sig
    type t

    structure Value : HPAXOS_VALUE
    type value = Value.t

    structure Ballot : HPAXOS_BALLOT
    type ballot = Ballot.t

    val hash : t -> word

    val is_one_a : t -> bool
    val is_one_b : t -> bool
    val is_two_a : t -> bool

    (* if the message is 1a, return its ballot; otherwise, return NONE *)
    val get_bal : t -> ballot option

    (* if the message is 1a, return its ballot; otherwise, return NONE *)
    val get_val : t -> value option

    (* returns a previous message of the sender *)
    val get_prev : t -> t option

    (* returns a list of direct references *)
    val get_refs : t -> t list
end

functor MessageOrdKey (Msg : HPAXOS_MESSAGE) : ORD_KEY =
struct
    type ord_key = Msg.t
    fun compare (m1, m2) = Word.compare (Msg.hash m1, Msg.hash m2)
end
