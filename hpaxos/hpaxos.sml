signature HPAXOS_NODE =
sig
    type t
    type node_id
    type learner_graph
    type mailbox
    val hpaxos_node : node_id -> learner_graph -> mailbox -> t
    val run : t -> unit
end

functor HPaxos (structure Msg : HPAXOS_MESSAGE
                structure Mailbox : HPAXOS_MAILBOX
                structure LearnerGraph : LEARNER_GRAPH
                sharing Msg.Learner = LearnerGraph.Learner
                    and Msg.Acceptor = LearnerGraph.Acceptor
                    and Mailbox.Message = Msg)
        :> HPAXOS_NODE =
struct
    type msg = Msg.t
    type mailbox = Mailbox.t

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

    structure LearnerMsgMap : ORD_MAP =
        RedBlackMapFn (
            ProdLexOrdKey
                (LearnerOrdKey (Msg.Learner))
                (MessageOrdKey (Msg)))

    (* algorithm state *)
    structure AlgoState =
    struct
        datatype known_msgs = KnownMsgs of MsgSet.set
        datatype recent_msgs = RecentMsgs of MsgSet.set
        datatype prev_msg = PrevMsg of msg option
        datatype non_wellformed_msgs = NonWellformedMsgs of MsgSet.set
        datatype max_ballot = MaxBal of ballot

        datatype state = AlgoState of known_msgs * recent_msgs * prev_msg *
                                      non_wellformed_msgs * max_ballot
        type t = state

        fun mk () = AlgoState (KnownMsgs MsgSet.empty,
                               RecentMsgs MsgSet.empty,
                               PrevMsg NONE,
                               NonWellformedMsgs MsgSet.empty,
                               MaxBal Msg.Ballot.zero)

        fun is_known (AlgoState (KnownMsgs k, _, _, _, _)) =
            Fn.curry MsgSet.member k

        fun add_known (AlgoState (KnownMsgs k, r, p, nw, maxb)) m =
            AlgoState (KnownMsgs (MsgSet.add (k, m)), r, p, nw, maxb)

        fun get_recent (AlgoState (_, RecentMsgs r, _, _, _)) = r

        fun add_recent (AlgoState (k, RecentMsgs r, p, nw, maxb)) m =
            AlgoState (k, RecentMsgs (MsgSet.add (r, m)), p, nw, maxb)

        fun clear_recent (AlgoState (k, _, p, nw, maxb)) =
            AlgoState (k, RecentMsgs MsgSet.empty, p, nw, maxb)

        fun get_prev (AlgoState (_, _, PrevMsg p, _, _)) = p

        fun set_prev (AlgoState (k, r, _, nw, maxb)) m =
            AlgoState (k, r, PrevMsg (SOME m), nw, maxb)

        fun is_non_wellformed (AlgoState (_, _, _, NonWellformedMsgs nw, _)) =
            Fn.curry MsgSet.member nw

        fun add_non_wellformed (AlgoState (k, r, p, NonWellformedMsgs nw, maxb)) m =
            AlgoState (k, r, p, NonWellformedMsgs (MsgSet.add (nw, m)), maxb)

        fun get_max (AlgoState (_, _, _, _, MaxBal maxb)) = maxb

        fun set_max (AlgoState (k, r, p, nw, _)) bal =
            AlgoState (k, r, p, nw, MaxBal bal)
    end (* AlgoState *)

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
        type info_entry = { info_bal_val : ballot * value,
                            info_W : (msg * msg option) LearnerAcceptorMap.map,
                            info_acc_status : AcceptorStatus.t AcceptorMap.map,
                            info_unburied_2as : MsgSet.set,
                            info_q : acceptor list }

        datatype msg_info = MsgInfo of info_entry MsgMap.map

        type t = msg_info

        fun mk () = MsgInfo MsgMap.empty

        fun get_bal_val (MsgInfo info) m = #info_bal_val (MsgMap.lookup (info, m))
        fun get_W (MsgInfo info) m = #info_W (MsgMap.lookup (info, m))
        fun get_acc_status (MsgInfo info) m = #info_acc_status (MsgMap.lookup (info, m))
        fun get_unburied_2as (MsgInfo info) m = #info_unburied_2as (MsgMap.lookup (info, m))
        fun get_q (MsgInfo info) m = #info_q (MsgMap.lookup (info, m))

        fun store_entry (MsgInfo info) (m, info_entry : info_entry) =
            MsgInfo (MsgMap.insert (info, m, info_entry))
    end (* MessageInfo *)

    (* memo state *)
    structure Cache =
    struct
        datatype is_fresh_cache = IsFresh of (bool LearnerMsgMap.map) ref
        datatype cache = Cache of is_fresh_cache
        type t = cache

        fun mk () = Cache (IsFresh (ref LearnerMsgMap.empty))

        fun get_is_fresh (Cache (IsFresh f)) = Fn.curry LearnerMsgMap.find (!f)
        fun put_is_fresh (Cache (IsFresh f)) (lm, v) =
            Ref.modify (fn map => LearnerMsgMap.insert (map, lm, v)) f
    end

    structure State =
    struct
        datatype state = State of AlgoState.t * MessageInfo.t * Cache.t
        type t = state

        fun mk () = State (AlgoState.mk (), MessageInfo.mk (), (Cache.mk ()))

        fun is_known (State (s, _, _)) = AlgoState.is_known s

        fun get_recent (State (s, _, _)) = AlgoState.get_recent s
        fun clear_recent (State (s, i, c)) = State (AlgoState.clear_recent s, i, c)

        fun get_prev (State (s, _, _)) = AlgoState.get_prev s
        fun set_prev (State (s, i, c)) m = State (AlgoState.set_prev s m, i, c)

        fun is_non_wellformed (State (s, _, _)) = AlgoState.is_non_wellformed s

        fun add_known (State (s, i, c)) m =
            State (AlgoState.add_known s m, i, c)

        fun add_recent (State (s, i, c)) m =
            State (AlgoState.add_recent s m, i, c)

        fun add_known_recent (State (s, i, c)) m =
            State (AlgoState.add_recent (AlgoState.add_known s m) m, i, c)

        fun add_non_wellformed (State (s, i, c)) m =
            State (AlgoState.add_non_wellformed s m, i, c)

        fun get_max (State (s, _, _)) = AlgoState.get_max s

        fun get_bal_val (State (s, i, c)) m =
            if Msg.is_one_a m then
                valOf (Msg.get_bal_val m)
            else
                MessageInfo.get_bal_val i m

        fun update_max (state as State (s, i, c)) m =
            let
                fun max (a, b) =
            end *)
                val m_bal = fst (get_bal_val state m)
                val cur_max = AlgoState.get_max s
                val new_max = max (m_bal, cur_max)
            in
                State (AlgoState.set_max s new_max, i, c)
            end

        fun get_W (State (_, i, _)) = MessageInfo.get_W i
        fun get_acc_status (State (_, i, _)) = MessageInfo.get_acc_status i
        fun get_unburied_2as (State (_, i, _)) = MessageInfo.get_unburied_2as i
        fun get_q (State (_, i, _)) = MessageInfo.get_q i

        fun store_info_entry (State (a, i, c)) mi =
            State (a, MessageInfo.store_entry i mi, c)

        fun get_is_fresh (State (_, _, c)) = Cache.get_is_fresh c
        fun put_is_fresh (State (_, _, c)) = Cache.put_is_fresh c
    end (* State *)

    type state = State.t

    (* learner graph *)
    datatype graph = Graph of learner_graph

    datatype acceptor_node = Acc of acceptor_id * graph * state * mailbox

    type t = acceptor_node
    type node_id = acceptor_id

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message, including 1a messages *)
    (* REQUIRES: m is not 1a *)
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
    (* REQUIRES: m is not 1a *)
    fun compute_W (m : msg) msg_to_bal_val msg_to_w
        : (msg * msg option) LearnerAcceptorMap.map =
        let
            fun pick_best_two_from_list (ms : msg list) : (msg * msg option) option =
                let
                    val ballot = fst o msg_to_bal_val
                    val value = snd o msg_to_bal_val
                    fun pick_best pred cmp lst =
                        let
                            fun choose (x, cur_best_o) =
                                if pred x then
                                    case cur_best_o of
                                        NONE => SOME x
                                      | SOME cur_best =>
                                        case cmp (cur_best, x) of
                                            LESS => SOME x
                                         |  _ => cur_best_o
                                else cur_best_o
                        in
                            foldl choose NONE lst
                        end
                    fun cmp_by_ballot (x, y) = Msg.Ballot.compare (ballot x, ballot y)
                    fun pick_first_best ms = pick_best (Fn.const true) cmp_by_ballot ms
                    fun pick_second_best ms fst_best =
                        let val fst_best_val = value fst_best
                            fun pred x = not (Msg.Value.eq (value x, fst_best_val))
                        in
                            pick_best pred cmp_by_ballot ms
                        end
                in
                    Option.map (fn x => (x, pick_second_best ms x)) (pick_first_best ms)
                end
            fun pick_best_two (a : msg * msg option, b : msg * msg option) =
                let fun to_list (best1, NONE) = [best1]
                      | to_list (best1, SOME best2) = [best1, best2]
                in
                    valOf (pick_best_two_from_list (to_list a @ to_list b))
                end
            fun join (r, w) = LearnerAcceptorMap.unionWith pick_best_two (msg_to_w r, w)
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
            foldl join w0 refs
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* REQUIRES: m is not 1a *)
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
    (* REQUIRES: m is not 1a *)
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
            val all_acceptors = LearnerGraph.acceptors g
            fun buried x =
                let
                    val x_lrn = valOf (Msg.learner x)
                    fun check acc =
                        case LearnerAcceptorMap.lookup (m_w, (x_lrn, acc)) of
                            (best1, o_best2) =>
                            burying (x, best1) orelse (isSome o_best2 andalso burying (x, (valOf o_best2)))
                    val acceptors = List.filter check all_acceptors
                in
                    LearnerGraph.is_quorum g (m_lrn, acceptors)
                end
        in
            MsgSet.filter (not o buried) u
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* [msg_to_unburied] returns a set MsgSet.set for each known message, excluding 1a *)
    (* REQUIRES: m is 2a *)
    fun compute_q (s : state) (m : msg) (g : learner_graph) msg_to_bal msg_to_acc_status msg_to_unburied
        : acceptor list =
        let
            fun compute_connected (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                let val caught =
                        AcceptorMap.listKeys (
                            AcceptorMap.filter AcceptorStatus.is_uncaught (msg_to_acc_status m)
                        )
                in
                    LearnerGraph.get_connected g (l, caught)
                end
            fun compute_connected_2as (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                let
                    val connected = LearnerSet.fromList (compute_connected (l, m))
                    val m_sender = Msg.sender m
                    fun pred x =
                        Msg.Acceptor.eq ((Msg.sender x), m_sender) andalso
                        LearnerSet.member (connected, valOf (Msg.learner x))
                in
                    MsgSet.filter pred (msg_to_unburied m)
                end
            fun is_fresh (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                    let
                        val connected_2as  = compute_connected_2as (l, m)
                        val m_bal = msg_to_bal m
                        fun from_this_sender x = Msg.Ballot.eq (msg_to_bal x, m_bal)
                    in
                        MsgSet.all from_this_sender connected_2as
                    end
            fun is_fresh' s (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                case State.get_is_fresh s (l, m) of
                    SOME b => b
                  | NONE =>
                    let val res = is_fresh (l, m)
                        val _ = State.put_is_fresh s ((l, m), res)
                    in
                        res
                    end
            val m_tran =
                let
                    val m_lrn = valOf (Msg.learner m)
                    val m_bal = msg_to_bal m
                    fun pred x = Msg.is_one_b x andalso is_fresh' s (m_lrn, x)
                    fun cont x = Msg.Ballot.eq (msg_to_bal x, m_bal)
                in
                    MsgUtil.tran pred cont m
                end
            fun senders ms =
                let fun helper (x, accu) = AcceptorSet.add' (Msg.sender x, accu) in
                    foldl helper AcceptorSet.empty ms
                end
        in
            AcceptorSet.toList (senders m_tran)
        end

    fun prev_wellformed (m : msg) : bool =
        let
            val m_refs = List.filter (not o Msg.is_one_a) (Msg.get_refs m)
            val m_acc = Msg.sender m
            fun from_this_sender x = Msg.Acceptor.eq ((Msg.sender x), m_acc)
        in
            case Msg.get_prev m of
                NONE => List.all (not o from_this_sender) m_refs
              | SOME prev =>
                isSome (List.find (Fn.curry Msg.eq prev) m_refs) andalso
                let fun check_ref x =
                        not (from_this_sender x) orelse Msg.eq (x, prev)
                in
                    List.all check_ref m_refs
                end
        end

    fun all_refs_known (s : state) (m : msg) =
        List.all (State.is_known s) (Msg.get_refs m)

    fun has_non_wellformed_ref s m =
        List.exists (State.is_non_wellformed s) (Msg.get_refs m)

    fun process_non_wellformed s m =
        State.add_non_wellformed s m
        (* ...further actions possible *)

    (* REQUIRES: every direct reference is known *)
    fun is_wellformed (s : state) (g : learner_graph) (m : msg) : bool * MessageInfo.info_entry option =
        let
            fun compute_msg_info_entry s m : MessageInfo.info_entry =
                (* REQUIRES: m is not 1a *)
                let
                    val get_bal_val = State.get_bal_val s
                    val get_W = State.get_W s
                    val get_acc_status = State.get_acc_status s
                    val get_unburied_2as = State.get_unburied_2as s
                    val m_bal_val = compute_bal_val m get_bal_val
                    fun get_bal_val_with_m x =
                        if Msg.eq (x, m) then m_bal_val else get_bal_val x
                    val m_W = compute_W m get_bal_val_with_m get_W
                    val m_acc_status =
                        compute_acceptor_status m (fst o get_bal_val_with_m) get_acc_status
                    val m_unburied_2as =
                        compute_unburied_2as m g get_bal_val_with_m get_W get_unburied_2as
                    val m_q =
                        if Msg.is_one_b m then [] else
                        (* case 2a *)
                        compute_q s m g (fst o get_bal_val_with_m) get_acc_status get_unburied_2as
                in
                    { info_bal_val = m_bal_val,
                      info_W = m_W,
                      info_acc_status = m_acc_status,
                      info_unburied_2as = m_unburied_2as,
                      info_q = m_q }
                end
            fun is_wellformed_1a m =
                (* TODO this check might be redundant depending on how `get_prev` is defined *)
                not (isSome (Msg.get_prev m)) andalso
                (* TODO this check might be redundant depending on how `get_refs` is defined *)
                null (Msg.get_refs m)
            fun is_wellformed_1b m (m_info_entry : MessageInfo.info_entry) =
                MsgUtil.references_exactly_one_1a m andalso
                let
                    val ballot = fst o (State.get_bal_val s)
                    val (m_bal, _) = #info_bal_val m_info_entry
                    fun check_ref x =
                        Msg.is_one_a x orelse
                        Msg.Ballot.compare (ballot x, m_bal) = LESS
                in
                    List.all check_ref (Msg.get_refs m)
                end
            fun is_wellformed_2a m (m_info_entry : MessageInfo.info_entry) =
                not (null (Msg.get_refs m)) andalso
                LearnerGraph.is_quorum g (valOf (Msg.learner m), #info_q m_info_entry)
        in
            (* optionally, we might want to check that every reference occurs at most once (call to refs_nondup) *)
            if prev_wellformed m andalso MsgUtil.refs_nondup m then
                case Msg.typ m of
                    Msg.OneA => (is_wellformed_1a m, NONE)
                  | Msg.OneB =>
                    let val m_info_entry = compute_msg_info_entry s m in
                        if is_wellformed_1b m m_info_entry then
                            (true, SOME m_info_entry)
                        else
                            (false, NONE)
                    end
                  | Msg.TwoA =>
                    let val m_info_entry = compute_msg_info_entry s m in
                        if is_wellformed_2a m m_info_entry then
                            (true, SOME m_info_entry)
                        else
                            (false, NONE)
                    end
            else (false, NONE)
        end

    fun check_wellformed_and_update_info (s : state) (g : learner_graph) m : bool * state =
        case is_wellformed s g m of
            (false, _) => (false, s)
          | (true, info_entry_o) =>
            (true, Option.fold (fn (e, s) => State.store_info_entry s (m, e)) s info_entry_o)

    fun hpaxos_node (id : node_id) (g : learner_graph) (mbox : mailbox) : t =
        Acc (id, Graph g, State.mk (), mbox)

    fun run (Acc (id, Graph g, s, mbox)) =
        let
            fun get_next_wellformed_msg s : msg * state =
                case Mailbox.recv mbox of
                    NONE => get_next_wellformed_msg s
                  | SOME m =>
                    let val _ = assert (all_refs_known s m) "wrong message ordering" in
                        if has_non_wellformed_ref s m then
                            get_next_wellformed_msg (process_non_wellformed s m)
                        else
                            let val (res, s) = check_wellformed_and_update_info s g m in
                                if res then (m, s) else
                                get_next_wellformed_msg (process_non_wellformed s m)
                            end
                    end
            fun process_1a s m : state =
                let
                    val prev = State.get_prev s
                    val recent = MsgSet.toList (State.get_recent s)
                    val new_1b = Msg.mk_one_b (prev, recent)
                    val (is_wf, s) = check_wellformed_and_update_info s g new_1b
                in
                    if is_wf then
                        let val s = State.clear_recent s
                            val s = State.set_prev s new_1b
                        in
                            process_message s new_1b
                        end
                    else s
                end
            and process_1b s m : state =
                let
                    val m_bal = fst (State.get_bal_val s m)
                    val cur_max = State.get_max s
                    fun process_learner (lrn, s) =
                        let
                            val prev = State.get_prev s
                            val recent = MsgSet.toList (State.get_recent s)
                            val new_2a = Msg.mk_two_a (prev, recent, lrn)
                            val (is_wf, s) = check_wellformed_and_update_info s g new_2a
                        in
                            if is_wf then
                                let val s = State.clear_recent s
                                    val s = State.set_prev s new_2a
                                in
                                    process_message s new_2a
                                end
                            else s
                        end
                in
                    if Msg.Ballot.eq (m_bal, cur_max) then
                        foldl process_learner s (LearnerGraph.learners g)
                    else s
                end
            and process_message s m =
                let
                    val _ = Mailbox.broadcast mbox m
                    val s = State.add_known s m
                    val s = State.add_recent s m
                    val s = State.update_max s m
                in
                    case Msg.typ m of
                        Msg.OneA => process_1a s m
                      | Msg.OneB => process_1b s m
                      | Msg.TwoA => s
                end
            fun loop s =
                let val (m, s) = get_next_wellformed_msg s in
                    if State.is_known s m then
                        loop s
                    else
                        loop (process_message s m)
                end
        in
            let val _ = loop s in () end
        end
end (* HPaxos *)
