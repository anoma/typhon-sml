signature NETWORK_MODEL_DISPATCHER =
sig
    structure Endpoint : NETWORK_MODEL_ENDPOINT
    type endpoint = Endpoint.t

    val dispatch : endpoint list -> unit -> unit
end
