functor RoundRobinBroadcastDispatcher (structure E : NETWORK_MODEL_ENDPOINT)
        : NETWORK_MODEL_DISPATCHER =
struct

val debug = false

type endpoint = E.t

fun dispatch endpoints () =
    let
        fun broadcast NONE = ()
          | broadcast (SOME msg) =
            app (fn e => E.put_inbound e msg) endpoints
        fun dispatch i =
            (* TODO syntax *)
            let
                val e = List.nth (endpoints, i)
                val msg = E.poll_outbound e
            in
                broadcast msg
            end
        fun doit i n =
            let val _ = dispatch i in
                doit (Int.mod (i + 1, n)) n
            end
    in
        doit 0 (length endpoints)
    end

end (* RoundRobinBroadcastDispatcher *)
