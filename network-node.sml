functor NetworkNode (structure S : NETWORK_SHIM)
        : NETWORK_NODE =
struct
    structure Config = NetworkConfig
    structure Shim = S

    type id = Config.node_id
    type shim = Shim.t
    type msg = Shim.msg
    type mailbox = msg Mailbox.mbox

    datatype node = Node of id * shim * mailbox
    type t = node

    fun listen node () =
        let
            val (Node (id, _, mbox)) = node
            val _ = TextIO.print (id ^ ": listening for new messages\n")
            val msg = Mailbox.recv mbox
        in
            TextIO.print
                (id ^ ": received message " ^ Shim.Msg.print msg ^ "\n");
            listen node ()
        end

    fun spawn (id, shim) =
        let
            val mbox = Mailbox.mailbox ()
            val _ = TextIO.print ("Created mailbox for node " ^ id ^ "\n")
            val this = Node (id, shim, mbox)
            val tid = CML.spawn (listen this)
        in
            TextIO.print (id ^ ": thread id " ^ CML.tidToString tid ^ "\n");
            (this, tid)
        end

    fun send (Node (_, _, mbox), msg) = Mailbox.send (mbox, msg)
end
