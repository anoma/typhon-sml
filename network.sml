functor Network (structure Shim : NETWORK_SHIM
                 structure Node : NETWORK_NODE
                 sharing Node.Shim = Shim)
        :> NETWORK =
struct
    type msg = Node.msg
    type node = Node.t

    type node_mailbox = Node.mailbox
    type t = { layer : Node.shim, nodes : (node * CML.thread_id) list }

    structure Config = Node.Config

    fun create (cfg : Config.t) =
        let
            val shim = Shim.create ()
            val _ = TextIO.print "Creating network shim\n"
            val _ = Shim.broadcast (shim, Shim.Msg.fromString "42")
            val node_ids = Config.node_ids cfg
            (* create node threads *)
            val nodes = map (fn id => Node.spawn (id, shim)) node_ids
            fun snd (_, x) = x
        in
            TextIO.print
                ("Created node threads: ["
                 ^ StringUtil.print_join CML.tidToString (map snd nodes)
                 ^ "]\n");
            { layer = shim, nodes = nodes }
        end

    fun run (net : t) () =
        let
            val msg = Shim.recv (#layer net)
        in
            (* forward the message to all nodes *)
            app (fn (node, _) => Node.send (node, msg)) (#nodes net);
            run net ()
        end

    fun launch (cfg : Config.t) () =
        let
            val _ = TextIO.print "Launching network with config\n"
            val _ = TextIO.print (Config.print cfg ^ "\n")
            val net = create cfg
            open CML
            (* spawn dispatcher thread *)
            val disp_tid = spawn (run net)
        in
            TextIO.print ("Dispatcher thread id " ^ tidToString disp_tid ^ "\n");
            (* join the network dispatcher thread and all the node threads *)
            TextIO.print "Waiting for everyone to terminate..\n";
            sync (joinEvt (disp_tid));
            TextIO.print "The dispatcher thread has terminated.\n";
            app (fn (_, node_tid) => sync (joinEvt (node_tid))) (#nodes net);
            TextIO.print "The node threads have terminated.\n"
        end
end
