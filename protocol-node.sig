signature PROTOCOL_NODE =
sig
    type param

    (* MUST *)
    structure Endpoint : NETWORK_MODEL_ENDPOINT
    type endpoint = Endpoint.t

    val launch : param -> CML.thread_id * endpoint
end
