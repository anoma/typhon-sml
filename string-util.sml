structure StringUtil =
struct
    fun print_join_sep (sep : string) (print : 'a -> string) (l : 'a list) =
        let
            fun doit accu [] = accu
              | doit accu [x] = accu ^ print x
              | doit accu (hd :: tl) = doit (accu ^ print hd ^ sep) tl
        in
            doit "" l
        end

    fun print_join print = print_join_sep ", " print

    val string_join = print_join (fn x => x)
end (* StringUtil *)
