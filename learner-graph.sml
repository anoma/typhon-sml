use "epoch.sml";

signature LEARNER =
sig
    type t
    val learner_id : word
end

signature LEARNER_GRAPH =
sig
    type t
    type epoch
end

functor LeanerGraph (structure Learner : LEARNER)
                    (structure Epoch : EPOCH)
        : LEARNER_GRAPH =
struct
    type epoch = Epoch.t
    datatype learner_graph = LearnerGraph of epoch
    type t = learner_graph
end
