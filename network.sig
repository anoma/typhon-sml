signature NETWORK =
sig
    type t
    type msg

    structure Config : NETWORK_CONFIG

    val launch : Config.t -> unit -> unit
end
