signature NETWORK_SHIM =
sig
    structure Msg : NETWORK_MESSAGE

    type t
    type msg = Msg.t

    val create : unit -> t
    val broadcast : t * msg -> unit
    val recv : t -> msg
end
