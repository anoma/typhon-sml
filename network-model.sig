(* TODO rename NETWORK_MODEL *)
signature NETWORK =
sig
    type param
    val create : param list -> unit
end
