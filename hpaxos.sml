use "hpaxos-message.sml";
use "learner-graph.sml";
use "util.sml";

signature HPAXOS_NODE =
sig
    type t
    type node_id
    type learner_graph
    val hpaxos_node : node_id -> learner_graph -> t
end

functor HPaxos (structure Msg : HPAXOS_MESSAGE
                structure LearnerGraph : LEARNER_GRAPH
                sharing Msg.Learner = LearnerGraph.Learner
                    and Msg.Acceptor = LearnerGraph.Acceptor) :> HPAXOS_NODE =
struct
    type msg = Msg.t

    type acceptor_id = word
    type ballot = Msg.Ballot.t
    type value = Msg.Value.t
    type learner_graph = LearnerGraph.t

    structure MsgUtil = MessageUtil (Msg)

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))
    structure AcceptorMap : ORD_MAP = RedBlackMapFn (AcceptorOrdKey (Msg.Acceptor))
    structure LearnerAcceptorMap : ORD_MAP =
        RedBlackMapFn (
            ProdLexOrdKey
                (LearnerOrdKey (Msg.Learner))
                (AcceptorOrdKey (Msg.Acceptor)))

    (* algorithm state *)
    datatype known_msgs = KnownMsgs of MsgSet.set
    datatype recent_msgs = RecentMsgs of MsgSet.set

    datatype state = State of known_msgs * recent_msgs

    (* message info state *)
    structure AcceptorStatus =
    struct
        datatype status = Caught
                        | Uncaught of msg
        type t = status

        fun join (Uncaught m1, Uncaught m2) =
            if MsgUtil.is_prev_reachable (m1, m2) then
                Uncaught m1
            else if MsgUtil.is_prev_reachable (m2, m1) then
                Uncaught m2
            else
                Caught
          | join (_, _) = Caught
    end

    datatype info_bal_val = InfoBalVal of (ballot * value) MsgMap.map
    datatype info_W = InfoW of ((msg * msg option) LearnerAcceptorMap.map) MsgMap.map
    datatype info_acc_status = InfoAccStatus of (AcceptorStatus.t AcceptorMap.map) MsgMap.map
    datatype info_unburied_2as = InfoUnburied of MsgSet.set MsgMap.map
    datatype msg_info = MsgInfo of info_bal_val * info_W * info_acc_status * info_unburied_2as

    (* memo state *)
    datatype cache = Cache of int

    (* learner graph *)
    datatype graph = Graph of learner_graph

    datatype acceptor = Acc of acceptor_id * graph * state * msg_info * cache

    type t = acceptor
    type node_id = acceptor_id

    fun is_known (m : msg) (State (KnownMsgs k, r)) : bool =
        MsgSet.member (k, m)

    fun add_known (m : msg) (State (KnownMsgs k, r)) : state =
        State (KnownMsgs (MsgSet.add (k, m)), r)

    fun add_recent (m : msg) (State (k, RecentMsgs r)) : state =
        State (k, RecentMsgs (MsgSet.add (r, m)))

    fun compute_bal_val (m : msg) (InfoBalVal info_bal_val) : ballot * value =
        if Msg.is_one_a m then
            valOf (Msg.get_bal_val m)
        else
            let
                fun helper (x, (max_bal, max_val)) =
                    let val (b, v) = MsgMap.lookup (info_bal_val, x) in
                        case Msg.Ballot.compare (b, max_bal) of
                            LESS => (max_bal, max_val)
                          | _ => (b, v)
                    end
                val refs = Msg.get_refs m (* refs is non-empty since m is not 1a *)
            in
                List.foldl helper (Msg.Ballot.zero, Msg.Value.default) refs
            end

    fun compute_W (m : msg) (InfoBalVal info_bal_val) (InfoW info_w)
        : (msg * msg option) LearnerAcceptorMap.map =
        let
            fun pick_best_two_from_list (ms : msg list) : (msg * msg option) option =
                let fun picker (x, NONE) = SOME (x, NONE)
                      | picker (x, SOME (best1, o_best2)) =
                        let
                            fun pick_second_best
                                    fst_best_val candidate (candidate_bal, candidate_val) cur_snd_best_option =
                                if Msg.Value.eq (candidate_val, fst_best_val) then
                                    cur_snd_best_option
                                else
                                    let val new_snd_best =
                                            case cur_snd_best_option of
                                                NONE => candidate
                                              | SOME cur_snd_best =>
                                                let val (bal2, _) = MsgMap.lookup (info_bal_val, cur_snd_best) in
                                                    case Msg.Ballot.compare (candidate_bal, bal2) of
                                                        GREATER => candidate
                                                      | _ => cur_snd_best
                                                end
                                    in
                                        SOME new_snd_best
                                    end
                            val (b, v) = MsgMap.lookup (info_bal_val, x)
                            val (bal1, val1) = MsgMap.lookup (info_bal_val, best1)
                            val new_best =
                                case Msg.Ballot.compare (b, bal1) of
                                    GREATER => (x, pick_second_best v best1 (bal1, val1) o_best2)
                                  | _ => (best1, pick_second_best val1 x (b, v) o_best2)
                        in
                            SOME new_best
                        end
                in
                    List.foldl picker NONE ms
                end
            fun pick_best_two (a : msg * msg option, b : msg * msg option) =
                let fun to_list (best1, NONE) = [best1]
                      | to_list (best1, SOME best2) = [best1, best2]
                in
                    valOf (pick_best_two_from_list (List.concat [to_list a, to_list b]))
                end
            fun helper (r, w) =
                let val r_w = MsgMap.lookup (info_w, r)
                in
                    LearnerAcceptorMap.unionWith pick_best_two (r_w, w)
                end
            val w0 =
                if Msg.is_two_a m then
                    LearnerAcceptorMap.insert
                        (LearnerAcceptorMap.empty,
                         (valOf (Msg.learner m), Msg.sender m),
                         (m, NONE))
                else
                    LearnerAcceptorMap.empty
        in
            List.foldl helper w0 (Msg.get_refs m)
        end

    fun compute_acceptor_status (m : msg) (InfoAccStatus info_acc_status)
        : AcceptorStatus.t AcceptorMap.map =
        (* assume: m is not 1a *)
        let
            fun helper (r, s) =
                let val r_acc_status = MsgMap.lookup (info_acc_status, r) in
                    AcceptorMap.unionWith AcceptorStatus.join (r_acc_status, s)
                end
            val s0 =
                if not (Msg.is_one_a m) then
                    AcceptorMap.singleton (Msg.sender m, AcceptorStatus.Uncaught m)
                else
                    AcceptorMap.empty
        in
            List.foldl helper s0 (Msg.get_refs m)
        end

    fun compute_unburied_2as (m : msg)
                             (Graph g)
                             (InfoBalVal info_bal_val)
                             (InfoW info_w)
                             (InfoUnburied info_unburied)
        : MsgSet.set =
        let
            val m_lrn = valOf (Msg.learner m)
            (* z is burying x *)
            fun burying (x, z) =
                Msg.is_two_a z andalso (* TODO check if redundant *)
                Msg.Learner.eq
                    (valOf (Msg.learner x), valOf (Msg.learner z)) andalso
                let
                    val (x_bal, x_val) = MsgMap.lookup (info_bal_val, x)
                    val (z_bal, z_val) = MsgMap.lookup (info_bal_val, z)
                in
                    Msg.Ballot.compare (x_bal, z_bal) = LESS andalso
                    not (Msg.Value.eq (x_val, z_val))
                end
            val u0 = if Msg.is_two_a m then MsgSet.singleton m else MsgSet.empty
            fun doit (r, u) =
                MsgSet.union (u, MsgMap.lookup (info_unburied, r))
            val u = List.foldl doit u0 (Msg.get_refs m)
            val m_w = MsgMap.lookup (info_w, m)
            val all_acceptors = LearnerGraph.all_acceptors g
            fun buried x =
                let
                    val x_lrn = valOf (Msg.learner x)
                    fun doit acc =
                        let val (best1, o_best2) = LearnerAcceptorMap.lookup (m_w, (x_lrn, acc)) in
                            burying (x, best1) orelse
                            (isSome o_best2 andalso burying (x, (valOf o_best2)))
                        end
                    val acceptors = List.filter doit all_acceptors
                in
                    LearnerGraph.is_quorum g (m_lrn, acceptors)
                end
        in
            MsgSet.filter (not o buried) u
        end

    fun hpaxos_node (id : node_id) (g : LearnerGraph.t) : t =
        Acc (id,
             Graph g,
             State (KnownMsgs MsgSet.empty,
                    RecentMsgs MsgSet.empty),
             MsgInfo (InfoBalVal MsgMap.empty,
                      InfoW MsgMap.empty,
                      InfoAccStatus MsgMap.empty,
                      InfoUnburied MsgMap.empty),
             Cache 0)
end
