signature NETWORK_SHIM =
sig
    type t
    type msg

    val shim : unit -> t
    val send : t -> msg -> unit
    val recv : t -> msg
end
