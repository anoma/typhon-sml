signature LEARNER_GRAPH =
sig
    type t

    structure Epoch : EPOCH
    type epoch = Epoch.t

    structure Learner : LEARNER
    type learner = Learner.t

    structure Acceptor : ACCEPTOR
    type acceptor = Acceptor.t

    val get_epoch : t -> epoch
    val all_acceptors : t -> acceptor list

    val is_quorum : t -> learner * acceptor list -> bool
    val get_connected : t -> learner * acceptor list -> learner list
end
