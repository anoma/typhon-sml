functor NetworkShim (M : NETWORK_MESSAGE) : NETWORK_SHIM =
struct
    structure Msg = M

    type msg = M.t

    datatype shim = Shim of msg Mailbox.mbox
    type t = shim

    fun create () = Shim (Mailbox.mailbox ())
    fun broadcast (Shim mbox, msg) = Mailbox.send (mbox, msg)
    fun recv (Shim mbox) = Mailbox.recv mbox
end
