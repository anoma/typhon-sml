structure Hashing = struct
    infix |>
    fun x |> f = f x

    fun hash words =
        words
        |> List.map Word.toString
        |> String.concat
        |> FNVHash.hashString
end
