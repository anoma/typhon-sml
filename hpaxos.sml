use "hpaxos-message.sml";

signature HPAXOS_NODE =
sig
    type t
    type node_id
    val hpaxos_node : node_id -> t
end

functor HPaxos (structure Msg : HPAXOS_MESSAGE) :> HPAXOS_NODE =
struct
    type acceptor_id = word
    type ballot = Msg.Ballot.t
    type value = Msg.Value.t

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))

    (* algorithm state *)
    datatype known_msgs = KnownMsgs of MsgSet.set
    datatype recent_msgs = RecentMsgs of MsgSet.set

    datatype state = State of known_msgs * recent_msgs

    (* cache state *)
    datatype msg_ballot_cache = MsgBallotCache of ballot MsgMap.map
    datatype msg_value_cache = MsgValueCache of value MsgMap.map
    datatype cache = Cache of msg_ballot_cache * msg_value_cache

    datatype acceptor = Acc of acceptor_id * state * cache

    type t = acceptor
    type node_id = acceptor_id
    type msg = Msg.t

    fun is_known (m : msg) (State (KnownMsgs(k), r)) : bool =
        MsgSet.member (k, m)

    fun add_known (m : msg) (State (KnownMsgs(k), r)) : state =
        State (KnownMsgs (MsgSet.add (k, m)), r)

    fun add_recent (m : msg) (State (k, RecentMsgs(r))) : state =
        State (k, RecentMsgs (MsgSet.add (r, m)))

    fun compute_ballot (m : msg) c : ballot * msg =
        if Msg.is_one_a m then
            (Option.valOf(Msg.get_bal m), m)
        else
            let
                val refs = Msg.get_refs m (* must be non-empty *)
                val Cache (MsgBallotCache(bc), _) = c
                fun helper (m, (max_bal, max_msg)) =
                    let val b = MsgMap.lookup (bc, m) in
                        case Msg.Ballot.compare (b, max_bal) of
                            LESS => (max_bal, max_msg)
                          | _ => (b, m)
                    end
            in
                List.foldr helper (Msg.Ballot.zero, m) refs
            end

    fun compute_and_store_ballot (m : msg) c : cache =
        let
            val (bal, _) = compute_ballot m c
            val Cache (MsgBallotCache(bc), vc) = c
        in
            Cache (MsgBallotCache (MsgMap.insert (bc, m, bal)), vc)
        end

    fun compute_value (m : msg) c : value =
        if Msg.is_one_a m then
            Option.valOf(Msg.get_val m)
        else
            let
                val (_, max_msg) = compute_ballot m c
                val Cache (_, MsgValueCache(vc)) = c
            in
                MsgMap.lookup (vc, max_msg)
            end

    fun compute_and_store_value (m : msg) c : cache =
        let
            val v = compute_value m c
            val Cache (bc, MsgValueCache(vc)) = c
        in
            Cache (bc, MsgValueCache (MsgMap.insert (vc, m, v)))
        end

    fun hpaxos_node (id : node_id) : t =
        Acc (id,
             State (KnownMsgs (MsgSet.empty),
                    RecentMsgs (MsgSet.empty)),
             Cache (MsgBallotCache (MsgMap.empty),
                    MsgValueCache (MsgMap.empty)))
end
