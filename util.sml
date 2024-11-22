
fun fst (x, _) = x
fun snd (_, x) = x

fun map_or (x : 'a option) (default : 'b) (f : 'a -> 'b) =
    case x of SOME v => f v | NONE => default

fun flip f x y = f y x

fun assert cond str =
    if cond then () else raise Fail ("assert " ^ str)

fun list_equal eq xs ys =
    let
        fun doit ([], []) = true
          | doit (x :: tx, y :: ty) =
                if eq (x, y) then doit (tx, ty) else false
          | doit (_, _) = false
    in
        doit (xs, ys)
    end

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
