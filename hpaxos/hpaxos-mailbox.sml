signature HPAXOS_MAILBOX =
sig
    type t
    structure Message : HPAXOS_MESSAGE

    val recv : t -> Message.t option
    val broadcast : t -> Message.t -> unit
end
