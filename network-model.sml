structure MessageMock =
struct
    type t = word
    fun hash x = x
    fun eq (x, y) =
        case Word.compare (x, y) of EQUAL => true | _ => false
    val print = Word.toString
    fun fromString str = case Word.fromString str of
                             SOME w => w
                           | NONE => Word.fromInt 0
end

structure ShimMock = NetworkShim (MessageMock)
structure NodeMock = NetworkNode (structure S = ShimMock)
structure NetworkMockModel = Network (structure Shim = ShimMock
                                      structure Node = NodeMock)

structure RunMockModel =
struct
    fun main (_, argv) =
        let
            val cfg : NetworkConfig.t = { node_ids = ["node0"] }
        in
            TextIO.print "Running...\n";
            RunCML.doit (NetworkMockModel.launch cfg, NONE)
        end
end
