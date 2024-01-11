structure NetworkConfig : NETWORK_CONFIG =
struct
    type node_id = string
    type t = { node_ids : node_id list }

    fun node_ids (conf : t) = #node_ids conf
    fun print (conf : t) =
        let open StringUtil in
            "[ " ^ string_join (#node_ids conf) ^ " ]"
        end
end
