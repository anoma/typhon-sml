(* HPaxos Message *)

use "learner.sml";
use "hpaxos-acceptor.sml";

signature HPAXOS_VALUE =
sig
    type t
    val default : t (* default value *)
    val eq : t * t -> bool (* equality *)
end

signature HPAXOS_BALLOT =
sig
    type t
    val zero : t (* the smallest ballot *)
    val eq : t * t -> bool
    val compare : t * t -> order
end

signature HPAXOS_MESSAGE =
sig
    type t

    structure Value : HPAXOS_VALUE
    type value = Value.t

    structure Ballot : HPAXOS_BALLOT
    type ballot = Ballot.t

    structure Learner : LEARNER
    type learner = Learner.t

    structure Acceptor : ACCEPTOR
    type acceptor = Acceptor.t

    val hash : t -> word
    val eq : t * t -> bool

    val is_one_a : t -> bool
    val is_one_b : t -> bool
    val is_two_a : t -> bool

    (* if the message is 2a, return its learner instance; otherwise, return NONE *)
    val learner : t -> learner option

    (* returns message sender *)
    val sender : t -> acceptor

    (* if the message is 1a, return its ballot and value; otherwise, return NONE *)
    val get_bal_val : t -> (ballot * value) option

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

functor MessageUtil (Msg : HPAXOS_MESSAGE) =
struct
    (* TODO can be optimized *)
    fun is_prev_reachable (m1, m2) =
        let fun doit NONE = false
              | doit (SOME m) =
                Msg.eq (m, m2) orelse doit (Msg.get_prev m)
        in
            doit (SOME m1)
        end

    (* DFS *)
    fun tran m (p : Msg.t -> bool) (cont : Msg.t -> bool) =
        let
            fun doit accu nil = accu
              | doit accu (x :: tl) =
                if cont x then
                    let val queue = (Msg.get_refs x) @ tl in
                        doit (if p x then x :: accu else accu) queue
                    end
                else
                    doit accu tl
        in
            doit [] [m]
        end
end
