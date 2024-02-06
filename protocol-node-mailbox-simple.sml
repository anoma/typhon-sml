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

datatype mbox = MBox of shim
type t = mbox

(* TODO serialization struct *)
fun decode msg = msg
fun encode msg = msg

fun recv (MBox shim) =
    let
        val raw_msg = Shim.recv shim
        val decoded_msg = decode raw_msg
    in
        decoded_msg
    end

fun send (MBox shim) msg =
    let
        val raw_msg = encode msg
    in
        Shim.send shim raw_msg
    end

fun mailbox (shim, param) = MBox shim

end (* PingPongMailbox *)
