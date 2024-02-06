functor Network (
    structure N : PROTOCOL_NODE
    structure D : NETWORK_MODEL_DISPATCHER
    sharing type N.Endpoint.t = D.endpoint
) : NETWORK_MODEL =
struct

fun snd (_, x) = x

type param = N.param

fun create node_params =
    (* TODO syntax *)
    let
        val nodes = map N.launch node_params
        val endpoints = map snd nodes
    in
        ignore(CML.spawn (D.dispatch endpoints))
    end
end (* Network *)
