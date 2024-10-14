signature HPAXOS_VALUE =
sig
    type t

    val default : t (* default value *)
    val eq : t * t -> bool (* equality *)
end
