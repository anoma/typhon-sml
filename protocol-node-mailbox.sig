signature PROTOCOL_NODE_MAILBOX =
sig
    type t

    (* MUST *)
    structure Msg : PROTOCOL_MESSAGE
    type msg = Msg.t

    (* MUST *)
    structure Shim : NETWORK_SHIM
    type shim = Shim.t

    val recv : t -> msg
    val send : t -> msg -> unit
    val mailbox : shim -> t
end
