
(* infix |> *)
(* fun x |> f = f x *)

functor ProtocolNode (structure Proc : PROTOCOL_PROCESSOR
                      structure Endpoint : NETWORK_MODEL_ENDPOINT
                      sharing type Proc.param = Proc.Mailbox.param
                          and type Endpoint.t = Proc.Mailbox.Shim.t)
        : PROTOCOL_NODE =
struct

type param = Proc.param

structure Endpoint = Endpoint
type endpoint = Endpoint.t

fun launch param =
    let
        val shim = Proc.Mailbox.Shim.shim ()
        val mailbox = Proc.Mailbox.mailbox (shim, param)
        val node = Proc.create (param, mailbox)
        val tid = CML.spawn (Proc.run node)
    in
        (tid, shim)
    end

end (* ProtocolNode *)
