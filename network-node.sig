signature NETWORK_NODE =
sig
    structure Config : NETWORK_CONFIG
    structure Shim : NETWORK_SHIM

    type t

    type id = Config.node_id
    type shim = Shim.t
    type msg = Shim.Msg.t
    type mailbox

    val spawn : id * shim -> t * CML.thread_id
    val send : t * msg -> unit
end
