signature HPAXOS_NODE =
sig
    type t
    type node_id
    type learner_graph
    type mailbox
    val hpaxos_node : node_id -> learner_graph -> mailbox -> t
end

functor HPaxos (structure Msg : HPAXOS_MESSAGE
                structure LearnerGraph : LEARNER_GRAPH
                sharing Msg.Learner = LearnerGraph.Learner
                    and Msg.Acceptor = LearnerGraph.Acceptor) :> HPAXOS_NODE =
struct
    type msg = Msg.t
    type mailbox = int

    type acceptor_id = word
    type acceptor = Msg.Acceptor.t
    type ballot = Msg.Ballot.t
    type value = Msg.Value.t
    type learner = Msg.Learner.t
    type learner_graph = LearnerGraph.t

    structure MsgUtil = MessageUtil (Msg)

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))
    structure AcceptorSet : ORD_SET = RedBlackSetFn (AcceptorOrdKey (Msg.Acceptor))
    structure AcceptorMap : ORD_MAP = RedBlackMapFn (AcceptorOrdKey (Msg.Acceptor))
    structure LearnerSet : ORD_SET = RedBlackSetFn (LearnerOrdKey (Msg.Learner))

    structure LearnerAcceptorMap : ORD_MAP =
        RedBlackMapFn (
            ProdLexOrdKey
                (LearnerOrdKey (Msg.Learner))
                (AcceptorOrdKey (Msg.Acceptor)))

    (* algorithm state *)
    structure AlgoState =
    struct
        datatype known_msgs = KnownMsgs of MsgSet.set
        datatype recent_msgs = RecentMsgs of MsgSet.set

        datatype state = AlgoState of known_msgs * recent_msgs
        type t = state

        fun init () = AlgoState (KnownMsgs MsgSet.empty, RecentMsgs MsgSet.empty)

        fun is_known (AlgoState (KnownMsgs k, _)) = curry MsgSet.member k

        fun is_recent (AlgoState (_, RecentMsgs r)) = curry MsgSet.member r

        fun add_known (AlgoState (KnownMsgs k, r)) (m : msg) : state =
            AlgoState (KnownMsgs (MsgSet.add (k, m)), r)

        fun add_recent (AlgoState (k, RecentMsgs r)) (m : msg) : state =
            AlgoState (k, RecentMsgs (MsgSet.add (r, m)))
    end

    (* message info state *)
    structure AcceptorStatus =
    struct
        datatype status = Caught
                        | Uncaught of msg
        type t = status

        fun is_uncaught (Uncaught _) = true
          | is_uncaught _ = false

        fun join (bal : msg -> ballot) (Uncaught m1, Uncaught m2) =
            if MsgUtil.PrevTran.is_prev_reachable' bal (m1, m2) then
                Uncaught m1
            else if MsgUtil.PrevTran.is_prev_reachable' bal (m2, m1) then
                Uncaught m2
            else
                Caught
          | join bal (_, _) = Caught
    end (* AcceptorStatus *)

    structure MessageInfo =
    struct
        datatype info_bal_val = InfoBalVal of (ballot * value)
        datatype info_W = InfoW of (msg * msg option) LearnerAcceptorMap.map
        datatype info_acc_status = InfoAccStatus of AcceptorStatus.t AcceptorMap.map
        datatype info_unburied_2as = InfoUnburied of MsgSet.set
        datatype info_q = InfoQ of acceptor list

        type info_all = info_bal_val * info_W * info_acc_status * info_unburied_2as * info_q
        datatype msg_info = MsgInfo of info_all MsgMap.map

        type t = msg_info

        fun init () = MsgInfo MsgMap.empty
        fun get_bal_val (MsgInfo info) m =
            case MsgMap.lookup (info, m) of
                (InfoBalVal bv, _, _, _, _) => bv

        fun get_W (MsgInfo info) m =
            case MsgMap.lookup (info, m) of
                (_, InfoW w, _, _, _) => w

        fun get_acc_status (MsgInfo info) m =
            case MsgMap.lookup (info, m) of
                (_, _, InfoAccStatus s, _, _) => s

        fun get_unburied_2as (MsgInfo info) m =
            case MsgMap.lookup (info, m) of
                (_, _, _, InfoUnburied u, _) => u

        fun get_q (MsgInfo info) m =
            case MsgMap.lookup (info, m) of
                (_, _, _, _, InfoQ q) => q
    end

    (* memo state *)
    structure Cache =
    struct
        datatype cache = Cache of int
        type t = cache

        fun init () = Cache 0
    end

    structure State =
    struct
        datatype state = State of AlgoState.t * MessageInfo.t * Cache.t
        type t = state

        fun init () = State (AlgoState.init (), MessageInfo.init (), Cache.init ())

        fun is_known (State (s, _, _)) = AlgoState.is_known s
        fun is_recent (State (s, _, _)) = AlgoState.is_recent s

        fun add_known (State (s, i, c)) (m : msg) =
            State (AlgoState.add_known s m, i, c)

        fun add_recent (State (s, i, c)) (m : msg) =
            State (AlgoState.add_recent s m, i, c)

        fun get_bal_val (State (s, i, c)) = MessageInfo.get_bal_val i
    end


    (* learner graph *)
    datatype graph = Graph of learner_graph

    datatype acceptor_node = Acc of acceptor_id * graph * State.t * mailbox

    type t = acceptor_node
    type node_id = acceptor_id

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message, including 1a messages *)
    (* ASSUME: m is not 1a *)
    fun compute_bal_val (m : msg) msg_to_bal_val : ballot * value =
        let
            fun helper (x, (max_bal, max_val)) =
                let val (b, v) = msg_to_bal_val x in
                    case Msg.Ballot.compare (b, max_bal) of
                        LESS => (max_bal, max_val)
                      | _ => (b, v)
                end
            val refs = Msg.get_refs m (* refs is non-empty since m is not 1a *)
        in
            foldl helper (Msg.Ballot.zero, Msg.Value.default) refs
        end

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message and the message m *)
    (* [msg_to_w] returns a (msg * msg option) LearnerAcceptorMap.map for each known message, excluding 1a *)
    (* ASSUME: m is not 1a *)
    fun compute_W (m : msg) msg_to_bal_val msg_to_w
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
                                                let val (bal2, _) = msg_to_bal_val cur_snd_best in
                                                    case Msg.Ballot.compare (candidate_bal, bal2) of
                                                        GREATER => candidate
                                                      | _ => cur_snd_best
                                                end
                                    in
                                        SOME new_snd_best
                                    end
                            val new_best =
                                let
                                    val (b, v) = msg_to_bal_val x
                                    val (bal1, val1) = msg_to_bal_val best1
                                in
                                    case Msg.Ballot.compare (b, bal1) of
                                        GREATER => (x, pick_second_best v best1 (bal1, val1) o_best2)
                                      | _ => (best1, pick_second_best val1 x (b, v) o_best2)
                                end
                        in
                            SOME new_best
                        end
                in
                    foldl picker NONE ms
                end
            fun pick_best_two (a : msg * msg option, b : msg * msg option) =
                let fun to_list (best1, NONE) = [best1]
                      | to_list (best1, SOME best2) = [best1, best2]
                in
                    valOf (pick_best_two_from_list (to_list a @ to_list b))
                end
            fun helper (r, w) = LearnerAcceptorMap.unionWith pick_best_two (msg_to_w r, w)
            val w0 =
                if Msg.is_two_a m then
                    LearnerAcceptorMap.insert
                        (LearnerAcceptorMap.empty,
                         (valOf (Msg.learner m), Msg.sender m),
                         (m, NONE))
                else
                    LearnerAcceptorMap.empty
            val refs = List.filter (not o Msg.is_one_a) (Msg.get_refs m)
        in
            foldl helper w0 refs
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* ASSUME: m is not 1a *)
    fun compute_acceptor_status (m : msg) msg_to_bal msg_to_acc_status
        : AcceptorStatus.t AcceptorMap.map =
        let
            fun helper (r, s) =
                AcceptorMap.unionWith (AcceptorStatus.join msg_to_bal) (msg_to_acc_status r, s)
            val s0 = AcceptorMap.singleton (Msg.sender m, AcceptorStatus.Uncaught m)
            val refs = List.filter (not o Msg.is_one_a) (Msg.get_refs m)
        in
            foldl helper s0 refs
        end

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message and the message m *)
    (* [msg_to_w] returns a (msg * msg option) LearnerAcceptorMap.map for each known message, excluding 1a *)
    (* [msg_to_unburied] returns a set MsgSet.set for each known message, excluding 1a *)
    (* ASSUME: m is not 1a *)
    fun compute_unburied_2as (m : msg) (g : learner_graph) msg_to_bal_val msg_to_w msg_to_unburied
        : MsgSet.set =
        let
            val m_lrn = valOf (Msg.learner m)
            (* z is burying x *)
            fun burying (x, z) =
                Msg.Learner.eq
                    (valOf (Msg.learner x), valOf (Msg.learner z)) andalso
                let
                    val (x_bal, x_val) = msg_to_bal_val x
                    val (z_bal, z_val) = msg_to_bal_val z
                in
                    Msg.Ballot.compare (x_bal, z_bal) = LESS andalso
                    not (Msg.Value.eq (x_val, z_val))
                end
            val u0 = if Msg.is_two_a m then MsgSet.singleton m else MsgSet.empty
            val refs = List.filter (not o Msg.is_one_a) (Msg.get_refs m)
            val u = foldl (fn (r, u) => MsgSet.union (msg_to_unburied r, u)) u0 refs
            val m_w = msg_to_w m
            val all_acceptors = LearnerGraph.all_acceptors g
            fun buried x =
                let
                    val x_lrn = valOf (Msg.learner x)
                    fun doit acc =
                        case LearnerAcceptorMap.lookup (m_w, (x_lrn, acc)) of
                            (best1, o_best2) =>
                            burying (x, best1) orelse (isSome o_best2 andalso burying (x, (valOf o_best2)))
                    val acceptors = List.filter doit all_acceptors
                in
                    LearnerGraph.is_quorum g (m_lrn, acceptors)
                end
        in
            MsgSet.filter (not o buried) u
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* [msg_to_unburied] returns a set MsgSet.set for each known message, excluding 1a *)
    (* ASSUME: m is 2a *)
    fun compute_q (m : msg) (g : learner_graph) msg_to_bal msg_to_acc_status msg_to_unburied
        : acceptor list =
        let
            fun compute_connected (l : learner, m : msg) =
                (* ASSUME: m is 1b *)
                let val caught =
                        AcceptorMap.listKeys (
                            AcceptorMap.filter AcceptorStatus.is_uncaught (msg_to_acc_status m)
                        )
                in
                    LearnerGraph.get_connected g (l, caught)
                end
            fun compute_connected_2as (l : learner, m : msg) =
                (* ASSUME: m is 1b *)
                let
                    val connected = LearnerSet.fromList (compute_connected (l, m))
                    val m_sender = Msg.sender m
                    fun pred x =
                        Msg.Acceptor.eq ((Msg.sender x), m_sender) andalso
                        LearnerSet.member (connected, valOf (Msg.learner x))
                in
                    MsgSet.filter pred (msg_to_unburied m)
                end
            fun is_fresh (l : learner, m : msg) = (* TODO cache the results *)
                (* ASSUME: m is 1b *)
                let
                    val connected_2as  = compute_connected_2as (l, m)
                    val m_bal = msg_to_bal m
                    fun from_this_sender x = Msg.Ballot.eq (msg_to_bal x, m_bal)
                in
                    MsgSet.all from_this_sender connected_2as
                end
            val m_tran =
                let
                    val m_lrn = valOf (Msg.learner m)
                    val m_bal = msg_to_bal m
                    fun pred x = Msg.is_one_b x andalso is_fresh (m_lrn, x)
                    fun cont x = Msg.Ballot.eq (msg_to_bal x, m_bal)
                in
                    MsgUtil.tran pred cont m
                end
            fun senders ms =
                let fun helper (x, accu) = AcceptorSet.add' (Msg.sender x, accu) in
                    foldl helper AcceptorSet.empty ms
                end
        in
            AcceptorSet.foldr (op ::) [] (senders m_tran)
        end

    fun prev_correct (m : msg) : bool =
        let
            val m_refs = List.filter (not o Msg.is_one_a) (Msg.get_refs m)
            val m_acc = Msg.sender m
            fun from_this_sender x = Msg.Acceptor.eq ((Msg.sender x), m_acc)
        in
            case Msg.get_prev m of
                NONE =>
                List.all (not o from_this_sender) m_refs
              | SOME prev =>
                isSome (List.find (curry Msg.eq prev) m_refs) andalso
                let fun check_ref x =
                        not (from_this_sender x) orelse Msg.eq (x, prev)
                in
                    List.all check_ref m_refs
                end
        end

    (* ASSUME: every direct reference is known *)
    fun is_wellformed (sigma : State.t) (m : msg) (m_bal : ballot) : bool * MessageInfo.info_all option =
        let fun is_wellformed_1a m =
                let val passed =
                        (* TODO this check might be redundant depending on how `get_prev` is defined *)
                        not (isSome (Msg.get_prev m)) andalso
                        (* TODO this check might be redundant depending on how `get_refs` is defined *)
                        null (Msg.get_refs m)
                in
                    (passed, NONE)
                end
            fun is_wellformed_1b m =
                let val passed =
                        (* MsgUtil.does_reference_1a m andalso *)
                        MsgUtil.references_exactly_one_1a m andalso
                        let fun check_ref x =
                                Msg.is_one_a x orelse
                                case State.get_bal_val sigma x of
                                    (bal, _) => Msg.Ballot.compare (bal, m_bal) = LESS
                        in
                            List.all check_ref (Msg.get_refs m)
                        end
                in
                    (passed, NONE)
                end
            fun is_wellformed_2a m =
                let val passed =
                        not (null (Msg.get_refs m)) andalso
                        false (* TODO check q *)
                in
                    (passed, NONE)
                end
        in
            if prev_correct m andalso
               (* optionally, we might want to check that every reference occurs at most once *)
               MsgUtil.refs_nondup m then
                case Msg.typ m of
                    Msg.OneA => is_wellformed_1a m
                  | Msg.OneB => is_wellformed_1b m
                  | Msg.TwoA => is_wellformed_2a m
            else (false, NONE)
        end

    fun hpaxos_node (id : node_id) (g : LearnerGraph.t) (inbox : mailbox) : t =
        Acc (id, Graph g, State.init(), inbox)
end
