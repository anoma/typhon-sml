functor ProtocolProcessor (structure Proto : PROTOCOL
                           structure Mailbox : PROTOCOL_NODE_MAILBOX
                           sharing Mailbox.Msg = Proto.Msg)
        : PROTOCOL_PROCESSOR =
struct

val debug = true

structure Param = Proto.Param
type param = Param.t

type state = Proto.state

structure Mailbox = Mailbox
type mailbox = Mailbox.t

datatype node = Node of {id : int, mbox : mailbox, state : state}
type t = node

fun create (p, mbox) =
    let
        val (init_msgs, init_state) = Proto.init p
        val _ = app (Mailbox.send mbox) init_msgs
    in
        Node {id = Param.id p, mbox = mbox, state = init_state}
    end

fun run (Node {id, mbox, state}) () =
    let
        val in_msg = Mailbox.recv mbox
        val _ =
            if debug then (
                TextIO.print
                    (Int.toString id
                     ^ ": state "
                     ^ Proto.State.toString state ^ "\n");
                TextIO.print
                    (Int.toString id
                     ^ ": processing message "
                     ^ Proto.Msg.toString in_msg ^ "\n")
            )
            else ()
        val (out_msgs, state') = Proto.process (in_msg, state)
        val _ =
            if debug then (
                TextIO.print
                    (Int.toString id
                     ^ ": new state "
                     ^ Proto.State.toString state' ^ "\n");
                TextIO.print
                    (Int.toString id
                     ^ ": sent messages "
                     ^ StringUtil.print_join Proto.Msg.toString out_msgs ^ "\n")
            )
            else ()
        val _ = app (Mailbox.send mbox) out_msgs
    in
        if Proto.terminate state' then
            ()
        else
            run (Node {id = id, mbox = mbox, state = state'}) ()
    end
end (* ProtocolProcessor *)
