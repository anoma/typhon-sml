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

    (* message info state *)
    datatype info_bal_val = InfoBalVal of ballot * value
    (* datatype info_W = InfoW of  *)
    datatype msg_info = MsgInfo of info_bal_val MsgMap.map

    (* memo state *)
    (* datatype msg_ballot_cache = MsgBallotCache of ballot MsgMap.map *)
    (* datatype msg_value_cache = MsgValueCache of value MsgMap.map *)
    (* datatype cache = Cache of msg_ballot_cache * msg_value_cache *)
    datatype cache = Cache of int

    datatype acceptor = Acc of acceptor_id * state * msg_info * cache

    type t = acceptor
    type node_id = acceptor_id
    type msg = Msg.t

    fun is_known (m : msg) (State (KnownMsgs(k), r)) : bool =
        MsgSet.member (k, m)

    fun add_known (m : msg) (State (KnownMsgs(k), r)) : state =
        State (KnownMsgs (MsgSet.add (k, m)), r)

    fun add_recent (m : msg) (State (k, RecentMsgs(r))) : state =
        State (k, RecentMsgs (MsgSet.add (r, m)))

    fun compute_bal_val (m : msg) (MsgInfo (info)) : ballot * value =
        if Msg.is_one_a m then
            Option.valOf (Msg.get_bal_val m)
        else
            let
                val refs = Msg.get_refs m (* refs is non-empty since m is not 1a *)
                fun helper (x, (max_bal, max_val)) =
                    let val InfoBalVal (b, v) = MsgMap.lookup (info, x) in
                        case Msg.Ballot.compare (b, max_bal) of
                            LESS => (max_bal, max_val)
                          | _ => (b, v)
                    end
            in
                List.foldr helper (Msg.Ballot.zero, Msg.Value.default) refs
            end

    fun compute_and_store_bal_val (m : msg) info : msg_info =
        let
            val bv = compute_bal_val m info
            val MsgInfo (info) = info
        in
            MsgInfo (MsgMap.insert (info, m, InfoBalVal(bv)))
        end

    fun hpaxos_node (id : node_id) : t =
        Acc (id,
             State (KnownMsgs (MsgSet.empty),
                    RecentMsgs (MsgSet.empty)),
             MsgInfo (MsgMap.empty),
             Cache (0))
end
