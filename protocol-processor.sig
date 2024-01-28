signature PROTOCOL_PROCESSOR =
sig
    type param
    type t

    (* type mailbox *)
    structure Mailbox : PROTOCOL_NODE_MAILBOX
    type mailbox = Mailbox.t

    val create : param * mailbox -> t
    val run : t -> unit -> unit
end
