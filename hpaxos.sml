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

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))

    (* algorithm state *)
    datatype known_msgs = KnownMsgs of MsgSet.set
    datatype recent_msgs = RecentMsgs of MsgSet.set

    datatype state = State of known_msgs * recent_msgs

    (* cache state *)
    datatype msg_ballot_cache = MsgBallotCache of ballot MsgMap.map
    datatype cache = Cache of msg_ballot_cache

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

    fun compute_and_store_ballot (m : msg) c  : cache =
        let
            val refs = Msg.get_refs m
        in
            if List.null refs then
                c
            else
                let
                    val Cache (MsgBallotCache(bs)) = c
                    fun helper (m, maxb) =
                        let val b = MsgMap.lookup (bs, m) in
                            case Msg.Ballot.compare (b, maxb) of
                                GREATER => b
                              | _ => maxb
                        end
                    val maxb = List.foldr helper Msg.Ballot.zero refs
                in
                    Cache (MsgBallotCache (MsgMap.insert (bs, m, maxb)))
                end
        end

    fun hpaxos_node (id : node_id) : t =
        Acc (id,
             State (KnownMsgs (MsgSet.empty),
                    RecentMsgs (MsgSet.empty)),
             Cache (MsgBallotCache (MsgMap.empty)))
end
