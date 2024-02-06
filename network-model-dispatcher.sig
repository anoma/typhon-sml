signature NETWORK_MODEL_DISPATCHER =
sig
    type endpoint

    val dispatch : endpoint list -> unit -> unit
end
