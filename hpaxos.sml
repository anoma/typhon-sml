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

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))

    datatype known_msgs = KnownMsgs of MsgSet.set
    datatype recent_msgs = RecentMsgs of MsgSet.set
    datatype state = State of known_msgs * recent_msgs

    datatype acceptor = Acc of acceptor_id * state

    type t = acceptor
    type node_id = acceptor_id
    type msg = Msg.t

    fun is_known (m : msg) (State (KnownMsgs(k), r)) : bool =
        MsgSet.member (k, m)

    fun add_known (m : msg) (State (KnownMsgs(k), r)) : state =
        State (KnownMsgs(MsgSet.add (k, m)), r)

    fun add_recent (m : msg) (State (k, RecentMsgs(r))) : state =
        State (k, RecentMsgs(MsgSet.add (r, m)))

    fun hpaxos_node (id : node_id) : acceptor =
        Acc(id,
            State(KnownMsgs(MsgSet.empty),
                  RecentMsgs(MsgSet.empty)))
end
