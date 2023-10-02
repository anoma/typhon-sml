use "epoch.sml";
use "learner.sml";

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
