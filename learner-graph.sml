use "epoch.sml";

signature LEARNER =
sig
    type t
    type learner_id = word
    val id : t -> learner_id
end

functor LearnerOrdKey (L : LEARNER) : ORD_KEY =
struct
    type ord_key = L.t
    fun compare (l1, l2) = Word.compare (L.id l1, L.id l2)
end

signature LEARNER_GRAPH =
sig
    type t
end

functor LearnerGraph (structure L : LEARNER)
                     (structure E : EPOCH)
        : LEARNER_GRAPH =
struct
    type epoch = E.t
    datatype learner_graph = LearnerGraph of epoch
    type t = learner_graph
    fun get_epoch (LearnerGraph e) = e
end
