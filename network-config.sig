signature NETWORK_CONFIG =
sig
    type t
    type node_id

    val node_ids : t -> node_id list
    val print : t -> string
end
