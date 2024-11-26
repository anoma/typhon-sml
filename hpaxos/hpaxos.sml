functor HPaxos (structure Msg : HPAXOS_MESSAGE
                structure LearnerGraph : LEARNER_GRAPH
                sharing Msg.Learner = LearnerGraph.Learner
                    and Msg.Acceptor = LearnerGraph.Acceptor)
        :> GEN_SERVER_IMPL =
struct
    infix |>
    fun x |> f = f x

    type acceptor = Msg.Acceptor.t
    type ballot = Msg.Ballot.t
    type value = Msg.Value.t
    type learner = Msg.Learner.t
    type learner_graph = LearnerGraph.t

    type msg = Msg.t

    structure MsgUtil = MessageUtil (Msg)

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))
    structure AcceptorSet : ORD_SET = RedBlackSetFn (AcceptorOrdKey (Msg.Acceptor))
    structure AcceptorMap : ORD_MAP = RedBlackMapFn (AcceptorOrdKey (Msg.Acceptor))
    structure LearnerSet : ORD_SET = RedBlackSetFn (LearnerOrdKey (Msg.Learner))
    structure LearnerMap : ORD_MAP = RedBlackMapFn (LearnerOrdKey (Msg.Learner))

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

        (* fun get_max (AlgoState (_, _, _, _, MaxBal maxb)) = maxb *)

        (* fun set_max (AlgoState (k, r, p, nw, _)) bal =
            AlgoState (k, r, p, nw, MaxBal bal) *)
    end (* AlgoState *)

    (* message info state *)
    structure AcceptorStatus =
    struct
        datatype status = Caught
                        | Uncaught of msg
        type t = status

        fun is_caught Caught = true
          | is_caught _ = false

        fun join (bal : msg -> ballot) (Uncaught m1, Uncaught m2) =
            if MsgUtil.PrevTran.is_prev_reachable' bal (m1, m2) then
                Uncaught m1
            else if MsgUtil.PrevTran.is_prev_reachable' bal (m2, m1) then
                Uncaught m2
            else
                Caught
          | join bal (_, _) = Caught
    end (* AcceptorStatus *)

    structure MessageType =
    struct
        datatype msg_type = OneA | OneB | TwoA
        type t = msg_type

        (* fun is_one_a OneA = true
          | is_one_a _ = false *)

        fun is_one_b OneB = true
          | is_one_b _ = false

        fun is_two_a TwoA = true
          | is_two_a _ = false
    end

    structure MessageInfo =
    struct
        type info_entry = { info_type : MessageType.t,
                            info_bal_val : ballot * value,
                            info_W : (msg * msg option) LearnerMap.map,
                            info_acc_status : AcceptorStatus.t AcceptorMap.map,
                            info_unburied_2as : MsgSet.set LearnerMap.map,
                            info_q : (acceptor list) LearnerMap.map,
                            info_learners : learner list }

        datatype msg_info = MsgInfo of info_entry MsgMap.map

        type t = msg_info

        fun mk () = MsgInfo MsgMap.empty

        fun has ((MsgInfo info), m) = MsgMap.inDomain (info, m)

        fun get_type (MsgInfo info) m = #info_type (MsgMap.lookup (info, m))

        (* fun is_one_a info m = get_type info m |> MessageType.is_one_a *)
        fun is_one_b info m = get_type info m |> MessageType.is_one_b
        fun is_two_a info m = get_type info m |> MessageType.is_two_a

        fun get_bal_val (MsgInfo info) m = #info_bal_val (MsgMap.lookup (info, m))
        fun get_learners (MsgInfo info) m = #info_learners (MsgMap.lookup (info, m))
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

        fun add_recent m (State (s, i, c)) =
            State (AlgoState.add_recent s m, i, c)

        fun get_prev (State (s, _, _)) = AlgoState.get_prev s
        fun set_prev m (State (s, i, c)) = State (AlgoState.set_prev s m, i, c)

        fun is_non_wellformed (State (s, _, _)) = AlgoState.is_non_wellformed s

        (* fun add_known (State (s, i, c)) m =
            State (AlgoState.add_known s m, i, c) *)

        fun add_non_wellformed (State (s, i, c)) m =
            State (AlgoState.add_non_wellformed s m, i, c)

        (* fun get_max (State (s, _, _)) = AlgoState.get_max s *)

        (* TODO clean *)
        (* fun is_one_a (State (_, i, _)) m = MessageInfo.is_one_a i m *)
        fun is_one_b (State (_, i, _)) m = MessageInfo.is_one_b i m
        (* TODO unused *)
        fun is_two_a (State (_, i, _)) m = MessageInfo.is_two_a i m

        fun get_bal_val (State (_, i, _)) m =
            if Msg.is_proposal m then
                valOf (Msg.get_bal_val m)
            else
                MessageInfo.get_bal_val i m

        (* fun update_max (state as State (s, i, c)) m =
            let
                fun max (a, b) =
                    case Msg.Ballot.compare (a, b) of LESS => b | _ => a
                val m_bal = fst (get_bal_val state m)
                val cur_max = AlgoState.get_max s
                val new_max = max (m_bal, cur_max)
            in
                State (AlgoState.set_max s new_max, i, c)
            end *)

        fun get_learners (State (_, i, _)) = MessageInfo.get_learners i
        fun get_W (State (_, i, _)) = MessageInfo.get_W i
        fun get_acc_status (State (_, i, _)) = MessageInfo.get_acc_status i
        fun get_unburied_2as (State (_, i, _)) = MessageInfo.get_unburied_2as i
        fun get_q (State (_, i, _)) = MessageInfo.get_q i

        fun has_info (State (_, i, _)) m = MessageInfo.has (i, m)

        fun store_info_entry (State (a, i, c)) mi =
            State (a, MessageInfo.store_entry i mi, c)

        fun get_is_fresh (State (_, _, c)) = Cache.get_is_fresh c
        fun put_is_fresh (State (_, _, c)) = Cache.put_is_fresh c
    end (* State *)

    structure ServerState =
    struct
        datatype state = State of learner_graph * State.t
        type t = state

        fun mk (g, s) = State (g, s)
    end (* ServerState *)

    type param = learner_graph
    type state = ServerState.t

    fun senders (ms : msg list) : acceptor list =
        let
            val empty = AcceptorSet.empty
            val add = AcceptorSet.add'
        in
            ms
            |> List.foldl (fn (x, accu) => add (Msg.sender x, accu)) empty
            |> AcceptorSet.toList
        end

    fun compute_type (m : msg) : MessageType.t =
        if Msg.is_proposal m then
            MessageType.OneA
        else
            let
                fun has_proposal_ref m =
                    Msg.get_refs m |> List.exists Msg.is_proposal
            in
                if has_proposal_ref m then
                    MessageType.OneB
                else
                    MessageType.TwoA
            end

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
        in
            (* refs is non-empty since m is not 1a *)
            Msg.get_refs m
            |> List.foldl helper (Msg.Ballot.zero, Msg.Value.default)
        end

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message and the message m *)
    (* [msg_to_w] returns a (msg * msg option) LearnerMap.map for each known message, excluding 1a *)
    (* REQUIRES: m is not 1a *)
    fun compute_W (m : msg) (m_type : MessageType.t) (g : learner_graph) msg_to_bal_val msg_to_w
        : (msg * msg option) LearnerMap.map =
        let
            val empty = LearnerMap.empty
            val insert = LearnerMap.insert
            val unionWith = LearnerMap.unionWith

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
                                          | _ => cur_best_o
                                else cur_best_o
                        in
                            List.foldl choose NONE lst
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
                    ms
                    |> pick_first_best
                    |> Option.map (fn x => (x, pick_second_best ms x))
                end
            fun pick_best_two (a : msg * msg option, b : msg * msg option) =
                let
                    fun to_list (best1, NONE) = [best1]
                      | to_list (best1, SOME best2) = [best1, best2]
                in
                    to_list a @ to_list b |> pick_best_two_from_list |> valOf
                end
            val w0 =
                if MessageType.is_two_a m_type then
                    LearnerGraph.learners g
                    |> List.foldl
                        (fn (alpha, u) => insert (u, alpha, (m, NONE)))
                        empty
                else
                    empty
        in
            Msg.get_refs m
            |> List.filter (not o Msg.is_proposal)
            |> List.foldl (fn (r, w) => unionWith pick_best_two (msg_to_w r, w)) w0
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* REQUIRES: m is not 1a *)
    fun compute_acceptor_status (m : msg) msg_to_bal msg_to_acc_status
        : AcceptorStatus.t AcceptorMap.map =
        let
            val unionWith = AcceptorMap.unionWith
            val join = AcceptorStatus.join msg_to_bal
            val s0 = AcceptorMap.singleton (Msg.sender m, AcceptorStatus.Uncaught m)
        in
            Msg.get_refs m
            |> List.filter (not o Msg.is_proposal)
            |> List.foldl (fn (r, s) => unionWith join (msg_to_acc_status r, s)) s0
        end

    (* [msg_to_bal_val] returns a pair (ballot, value) for each known message and the message m *)
    (* [msg_to_w] returns a (msg * msg option) LearnerMap.map for each known message, excluding 1a *)
    (* [msg_to_unburied] returns a set MsgSet.set for each known message, excluding 1a *)
    (* REQUIRES: m is not 1a *)
    fun compute_unburied_2as
            (m : msg) (g : learner_graph) (m_type : MessageType.t) m_W
            (msg_to_bal_val : msg -> ballot * value)
            (* TODO check if msg_to_learners has to be defined for m *)
            msg_to_learners
            msg_to_unburied
        : MsgSet.set LearnerMap.map =
        let
            fun is_learner_of (alpha : learner, x : msg) =
                msg_to_learners x
                |> List.find (Fn.curry Msg.Learner.eq alpha)
                |> Option.isSome

            (* z is burying x *)
            fun burying (alpha : learner) (x, z) =
                is_learner_of (alpha, z) andalso
                let
                    val (x_bal, x_val) = msg_to_bal_val x
                    val (z_bal, z_val) = msg_to_bal_val z
                in
                    Msg.Ballot.compare (x_bal, z_bal) = LESS andalso
                    not (Msg.Value.eq (x_val, z_val))
                end

            val get_w = Fn.curry LearnerMap.lookup m_W
            val all_acceptors = LearnerGraph.acceptors g

            fun compute_unburied_2as_for_learner (beta : learner) =
                let
                    val (best1, o_best2) = get_w beta 
                    fun buried x =
                        burying beta (x, best1) orelse
                        (isSome o_best2 andalso burying beta (x, (valOf o_best2)))
                    val u0 = if MessageType.is_two_a m_type then MsgSet.singleton m else MsgSet.empty
                in
                    Msg.get_refs m
                    |> List.filter (not o Msg.is_proposal)
                    |> List.foldl
                        (fn (r, u) =>
                            MsgSet.union (LearnerMap.lookup (msg_to_unburied r, beta), u))
                        u0
                    |> MsgSet.filter (not o buried)
                end
        in
            LearnerGraph.learners g
            |> List.foldl
                (fn (beta, u) =>
                    LearnerMap.insert (u, beta, compute_unburied_2as_for_learner beta))
                LearnerMap.empty
        end

    (* [msg_to_bal] returns a ballot for each known message, excluding 1a, and the message m *)
    (* [msg_to_acc_status] returns a map (AcceptorStatus.t AcceptorMap.map) for each known message, excluding 1a *)
    (* [msg_to_unburied] returns a set MsgSet.set for each known message, excluding 1a *)
    (* REQUIRES: m is 2a *)
    fun compute_q (s : State.t) (m : msg) (g : learner_graph)
                    msg_to_bal_val
                    msg_to_learners
                    msg_to_acc_status
                    msg_to_unburied
        : (acceptor list) LearnerMap.map =
        let
            val empty = LearnerMap.empty
            val insert = LearnerMap.insert

            val msg_to_bal = fst o msg_to_bal_val
            val msg_to_val = snd o msg_to_bal_val

            fun compute_connected (l : learner, m : msg) : learner list =
                (* REQUIRES: m is 1b *)
                let
                    val caught =
                        msg_to_acc_status m
                        |> AcceptorMap.filter AcceptorStatus.is_caught
                        |> AcceptorMap.listKeys
                in
                    (* TODO explore optimization options, e.g., caching *)
                    LearnerGraph.get_connected g (l, caught)
                end

            fun compute_connected_2as (alpha : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                let
                    val m_acc = Msg.sender m
                    val connected = compute_connected (alpha, m) |> LearnerSet.fromList
                    fun from_this_sender x = Msg.Acceptor.eq ((Msg.sender x), m_acc)
                in
                    m
                    |> msg_to_learners
                    |> List.filter (fn beta => LearnerSet.member (connected, beta))
                    |> List.foldl
                        (fn (beta, accu) =>
                            MsgSet.union (accu, LearnerMap.lookup (msg_to_unburied m, beta)))
                        MsgSet.empty
                    |> MsgSet.filter from_this_sender
                end

            fun is_fresh (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                let
                    val m_val = msg_to_val m
                    fun same_value x = Msg.Value.eq (msg_to_val x, m_val)
                in
                    compute_connected_2as (l, m) |> MsgSet.all same_value
                end
            (* cached `is_fresh` predicate, impure *)
            fun is_fresh' s (l : learner, m : msg) =
                (* REQUIRES: m is 1b *)
                case State.get_is_fresh s (l, m) of
                    SOME b => b
                  | NONE =>
                    let
                        val res = is_fresh (l, m)
                        val _ = State.put_is_fresh s ((l, m), res)
                    in
                        res
                    end

            fun compute_q_for_learner (alpha : learner) =
                let
                    val m_bal = msg_to_bal m
                    fun pred x = State.is_one_b s x andalso is_fresh' s (alpha, x)
                    fun cont x = Msg.Ballot.eq (msg_to_bal x, m_bal)
                in
                    m
                    |> MsgUtil.tran pred cont
                    |> senders
                end
        in
            LearnerGraph.learners g
            |> List.map (fn alpha => (alpha, compute_q_for_learner alpha))
            |> List.filter (fn (alpha, xs) => not (null xs))
            |> List.foldl
                (fn ((alpha, xs), accu) => insert (accu, alpha, xs))
                empty
        end

    (* REQUIRES: m is not 1a *)
    fun compute_msg_info_entry (s : State.t) (g : learner_graph) (m : msg)
            get_bal_val
            get_learners
            get_W
            get_acc_status
            get_unburied_2as
        : MessageInfo.info_entry =
        let
            val m_type = compute_type m
            val m_bal_val = compute_bal_val m get_bal_val
            fun get_bal_val_with_m x =
                if Msg.eq (x, m) then m_bal_val else get_bal_val x
            (* TODO rename W to something more meaningful *)
            (* compute W values as per message m *)
            val m_W = compute_W m m_type g get_bal_val_with_m get_W
            (* compute acceptor status as per message m *)
            val m_acc_status =
                compute_acceptor_status m (fst o get_bal_val_with_m) get_acc_status
            (* compute a set of unburied 2a-messages as per message m *)
            val m_unburied_2as =
                compute_unburied_2as m g m_type m_W get_bal_val_with_m get_learners get_unburied_2as
            (* list of quorums per each learner *)
            val m_q =
                if MessageType.is_one_b m_type then
                    LearnerMap.empty
                else
                    compute_q s m g get_bal_val_with_m get_learners get_acc_status get_unburied_2as
            (* list of learner values *)
            val m_learners = m_q |> LearnerMap.listKeys
        in
            {
                info_type = m_type,
                info_bal_val = m_bal_val,
                info_W = m_W,
                info_acc_status = m_acc_status,
                info_unburied_2as = m_unburied_2as,
                info_q = m_q,
                info_learners = m_learners
            }
        end

    (* I implement a stricter version of ChainRef predicate. Namely:
       - If the given message mentions a previous message,
       I check that the refs contains the mentioned previous message
       an no other message from the same sender.
       - If the given message claims that there was no previous message,
       I check that no message from the same sender is referenced. *)
    fun chain_ref (m : msg) : bool =
        let
            val m_refs = List.filter (not o Msg.is_proposal) (Msg.get_refs m)
            val m_acc = Msg.sender m
            fun from_this_sender x = Msg.Acceptor.eq ((Msg.sender x), m_acc)
        in
            case Msg.get_prev m of
                NONE => List.all (not o from_this_sender) m_refs
              | SOME prev =>
                isSome (List.find (Fn.curry Msg.eq prev) m_refs) andalso
                let
                    fun check_ref x =
                        not (from_this_sender x) orelse Msg.eq (x, prev)
                in
                    List.all check_ref m_refs
                end
        end

    fun all_refs_known (s : State.t) (m : msg) =
        List.all (State.is_known s) (Msg.get_refs m)

    fun has_non_wellformed_ref s m =
        List.exists (State.is_non_wellformed s) (Msg.get_refs m)

    fun process_non_wellformed s m =
        State.add_non_wellformed s m
        (* ...further actions possible *)

    (* REQUIRES: every direct reference is known *)
    fun is_wellformed (s : State.t) (g : learner_graph) (m : msg) : bool * MessageInfo.info_entry option =
        let
            val get_bal_val = State.get_bal_val s
            val get_learners = State.get_learners s
            val get_W = State.get_W s
            val get_acc_status = State.get_acc_status s
            val get_unburied_2as = State.get_unburied_2as s

            fun is_wellformed_1b m (m_info_entry : MessageInfo.info_entry) =
                MsgUtil.references_exactly_one_1a m andalso
                let
                    val ballot = fst o get_bal_val
                    val (m_bal, _) = #info_bal_val m_info_entry
                    fun check_ref x =
                        Msg.is_proposal x orelse
                        Msg.Ballot.compare (ballot x, m_bal) = LESS
                in
                    Msg.get_refs m |> List.all check_ref
                end

            fun is_wellformed_2a m (m_info_entry : MessageInfo.info_entry) =
                not (null (Msg.get_refs m)) andalso
                not (null (#info_learners m_info_entry))

            fun check_is_wellformed m (m_info_entry : MessageInfo.info_entry) =
                if MessageType.is_one_b (#info_type m_info_entry) then
                    is_wellformed_1b m m_info_entry
                else
                    is_wellformed_2a m m_info_entry

            fun is_wellformed_acceptor_message m =
                (* optionally, we might want to check that every reference occurs at most once (call to refs_nondup) *)
                (* TODO actually, do it in the mailbox (receiver) implementation *)
                if MsgUtil.refs_nondup m andalso chain_ref m then
                    let
                        val m_info_entry =
                            compute_msg_info_entry s g m
                                get_bal_val get_learners get_W get_acc_status get_unburied_2as
                    in
                        if check_is_wellformed m m_info_entry then
                            (true, SOME m_info_entry)
                        else
                            (false, NONE)
                    end
                else
                    (false, NONE)
        in
            if Msg.is_proposal m then
                (true, NONE)
            else
                is_wellformed_acceptor_message m
        end

    fun check_wellformed_and_update_info (s : State.t, g : learner_graph, m : msg)
        : bool * State.t =
        (* first, check if the message info is already stored, meaning that the message was sent by us *)
        (* TODO currently, broken; to make it really work, we need to use hashes as keys *)
        if State.has_info s m then
            (true, s)
        else
            case is_wellformed s g m of
                (false, _) => (false, s)
              | (true, info_entry_o) =>
                    (true, Option.fold
                                (fn (e, s) => State.store_info_entry s (m, e))
                                s
                                info_entry_o)

    fun init (g : learner_graph) = ServerState.mk (g, State.mk())

    fun handle_msg (ServerState.State (g, s), m) =
        let
            fun process s m : State.t * msg option =
                let
                    val prev = State.get_prev s
                    val recent = MsgSet.add (State.get_recent s, m) |> MsgSet.toList
                    val new = Msg.mk_msg (prev, recent)
                    val (is_wf, s) = check_wellformed_and_update_info (s, g, new)
                in
                    if is_wf then
                        let
                            fun update_state s =
                                s
                                |> State.clear_recent
                                |> State.add_recent new
                                |> State.set_prev new
                        in
                            (update_state s, SOME new)
                        end
                    else
                        let
                            val s = if not (Msg.is_proposal m) then State.add_recent m s else s
                        in
                            (s, NONE)
                        end
                end
        in
            let
                val (is_wf, s) =
                    if has_non_wellformed_ref s m then
                        (false, process_non_wellformed s m)
                    else
                        check_wellformed_and_update_info (s, g, m)
                val (s, new_msg) =
                    if is_wf then
                        process s m
                    else
                        (process_non_wellformed s m, NONE)
            in
                (ServerState.mk (g, s), new_msg)
            end
        end
end (* HPaxos *)
