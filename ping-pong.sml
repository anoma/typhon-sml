
structure PPP = PingPongProtocol
structure Shim = TwoMailboxNetworkShim (PPP.Msg)
structure MBox = SimpleProtocolNodeMailbox(
    structure ProtoMsg = PPP.Msg
    structure Shim = Shim
)
structure PingPongProcessor = ProtocolProcessor(
    structure Proto = PPP
    structure Mailbox = MBox
)
structure PingPongNode = ProtocolNode(
    structure Proc = PingPongProcessor
    structure Endpoint = Shim
)
structure RRBD = RoundRobinBroadcastDispatcher(structure E = Shim)
structure N = Network(
    structure N = PingPongNode
    structure D = RRBD
)

structure PingPong =
struct

fun run_ping_pong () =
    let
        val params =
            [ PPP.Param.Param {id=0, leader=true, bound=10},
              PPP.Param.Param {id=1, leader=false, bound=10} ]
    in
        N.create params
    end

fun main (_, argv) =
    RunCML.doit (run_ping_pong, NONE)

end (* PingPong *)
