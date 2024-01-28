signature NETWORK_MODEL_ENDPOINT =
sig
    type t
    type msg

    val put_inbound : t -> msg -> unit
    val poll_outbound : t -> msg option
end
