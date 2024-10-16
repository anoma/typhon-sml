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

    val mk_one_b : t option * t list -> t
    val mk_two_a : t option * t list * learner list -> t

    (* if the message is 2a, return a sorted list of learner instance for which quorums are defined;
       otherwise, return an empty list. *)
    val learners : t -> learner list

    (* returns message sender *)
    val sender : t -> acceptor

    (* if the message is 1a, return its ballot and value; otherwise, return NONE *)
    val get_bal_val : t -> (ballot * value) option

    (* returns a previous message of the sender *)
    val get_prev : t -> t option

    (* returns a list of direct references *)
    val get_refs : t -> t list
end
