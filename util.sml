
fun fst (x, _) = x
fun snd (_, x) = x

fun assert cond str =
    if cond then () else raise Fail ("assert " ^ str)

functor ProdLexOrdKey (A : ORD_KEY)
                      (B : ORD_KEY)
        : ORD_KEY =
struct
    type ord_key = A.ord_key * B.ord_key
    fun compare ((a1, b1), (a2, b2)) =
        case A.compare (a1, a2) of
            EQUAL => B.compare (b1, b2)
          | r => r
end
