functor TwoMailboxNetworkShim (M : NETWORK_MESSAGE)
        : sig
            include NETWORK_SHIM
            include NETWORK_MODEL_ENDPOINT
          end =
struct
    type msg = M.t
    type t = { inbound : msg Mailbox.mbox,
               outbound : msg Mailbox.mbox }

    fun shim () =
        let open Mailbox in
            { inbound = mailbox (), outbound = mailbox () }
        end

    fun send {inbound, outbound} msg =
        Mailbox.send (outbound, msg)

    fun recv {inbound, outbound} =
        Mailbox.recv inbound

    fun poll_outbound {inbound, outbound} =
        Mailbox.recvPoll outbound

    fun put_inbound {inbound, outbound} msg =
        Mailbox.send (inbound, msg)
end
