functor SimpleProtocolNodeMailbox (structure ProtoMsg : PROTOCOL_MESSAGE
                                   structure ProtoParam : PROTOCOL_PARAM
                                   structure Shim : NETWORK_SHIM
                                   sharing type ProtoMsg.t = Shim.msg)
        : PROTOCOL_NODE_MAILBOX =
struct

structure Param = ProtoParam
type param = Param.t

structure Msg = ProtoMsg
type msg = Msg.t

structure Shim = Shim
type shim = Shim.t

type net_msg = Shim.msg

datatype mbox = MBox of shim * int
type t = mbox

(* TODO serialization struct *)
fun decode msg = msg
fun encode msg = msg

fun recv (MBox (shim, id)) =
    let
        fun doit () =
            let
                val raw_msg = Shim.recv shim
                val decoded_msg = decode raw_msg
                val msg_sender = Msg.sender decoded_msg
            in
                if msg_sender = id then
                    doit ()
                else
                    decoded_msg
            end
    in
        doit ()
    end

fun send (MBox (shim, _)) msg =
    let
        val raw_msg = encode msg
    in
        Shim.send shim raw_msg
    end

fun mailbox (shim, param) = MBox (shim, Param.id param)

end (* PingPongMailbox *)
