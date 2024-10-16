signature GEN_SERVER_IMPL =
sig
    type param
    type state
    type msg

    val init : param -> state
    val handle_msg : state * msg -> state * msg option
end
