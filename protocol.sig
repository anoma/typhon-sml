signature PROTOCOL =
sig
    structure Param : PROTOCOL_PARAM
    type param = Param.t

    structure Msg : PROTOCOL_MESSAGE
    type msg = Msg.t

    structure State : PROTOCOL_STATE
    type state = State.t

    val init : param -> msg list * state
    val process : msg * state -> msg list * state
    val terminate : state -> bool
end
