functor Network (
    structure N : PROTOCOL_NODE
    structure D : NETWORK_MODEL_DISPATCHER
    sharing N.Endpoint = D.Endpoint
) : NETWORK =
struct

fun snd (_, x) = x

(* structure Endpoint = Node.Endpoint *)
type param = N.param

fun create node_params =
    (* TODO syntax *)
    let
        val nodes = map N.launch node_params
        val endpoints = map snd nodes
    in
        ignore(CML.spawn (D.dispatch endpoints))
    end
end
