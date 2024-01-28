structure PingPongProtocol =
struct

structure PingPongMessage =
struct
datatype ping_pong_msg =
         Ping of int * int |
         Pong of int * int
type t = ping_pong_msg

fun hash m = Word.fromInt 0

fun eq (Ping (u, v), Ping (x, y)) = (u = x) andalso (v = y)
  | eq (Pong (u, v), Pong (x, y)) = (u = x) andalso (v = y)
  | eq (_, _) = false

fun toString (Ping (id, seq)) =
    "Ping " ^ Int.toString seq ^ " (from " ^ Int.toString id ^ ")"
  | toString (Pong (id, seq)) =
    "Pong " ^ Int.toString seq ^ " (from " ^ Int.toString id ^ ")"

exception ParseError

fun fromString _ = Ping (0, 0)

end (* PingPongMessage *)

structure Msg = PingPongMessage


structure State =
struct

datatype state = State of {id : int, counter : int, bound : int}
type t = state

fun toString (State {id, counter, bound}) =
    "State "
    ^ "{ id: " ^ Int.toString id
    ^ ", counter: " ^ Int.toString counter
    ^ ", bound: " ^ Int.toString bound ^ " }"

end (* PingPongState *)


structure Param =
struct

datatype param = Param of {id : int, leader : bool, bound : int}
type t = param

fun id (Param {id, leader, bound}) = id

fun toString (Param {id, leader, bound}) =
    "Param "
    ^ "{ id: " ^ Int.toString id
    ^ ", leader: " ^ Bool.toString leader
    ^ ", bound: " ^ Int.toString bound ^ " }"

end (* PingPongParam *)


type msg = PingPongMessage.t
type state = State.t

type param = Param.t

fun init param  =
    let
        open PingPongMessage
        open Param
        open State
        val Param {id, leader, bound} = param
        val init_msgs = if leader then [Ping (id, 0)] else []
        val init_state = State {id = id, counter = 0, bound = bound}
    in
        (init_msgs, init_state)
    end

fun process (m, state) =
    let
        open PingPongMessage
        fun sender (Ping (id, _)) = id
          | sender (Pong (id, _)) = id
        open State
        val State {id, counter, bound} = state
    in
        if sender m = id then
            ([], state)
        else
            (case m of Ping (_, seq) => [Pong (id, seq)]
                     | Pong (_, seq) => [Ping (id, seq + 1)],
             State {id = id, counter = counter + 1, bound = bound})
    end

fun terminate state =
    let
        open State
        val State {id, counter, bound} = state
    in
        counter >= bound
    end

end (* PingPong *)
