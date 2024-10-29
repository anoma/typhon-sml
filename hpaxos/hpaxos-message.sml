structure HPaxosMessage (*: HPAXOS_MESSAGE*) =
struct
    infix |>
    fun x |> f = f x

    type hash = word

    structure Learner = Learner
    type learner = Learner.t

    structure Value = HPaxosValue
    type value = Value.t

    structure Ballot = HPaxosBallot
    type ballot = Ballot.t

    structure Acceptor = HPaxosAcceptor
    type acceptor = Acceptor.t

    datatype msg =
        Proposal of (
                ballot *        (* ballot *)
                hash            (* message hash *)
                )
        | NonProposal of (
                acceptor *      (* sender *)
                msg list *      (* references *)
                msg option *    (* previous message *)
                hash            (* message hash *)
                )
    
    type t = msg

    fun hash (Proposal (_, h) | NonProposal (_, _, _, h)) = h

    fun compute_hash (Proposal (bal, _)) =
        Ballot.hash bal
      | compute_hash (NonProposal (_, refs, prev, _)) =
        let
            val refs_hash = List.map hash refs
            val prev_hash = map_or prev [] (fn p => [hash p])
        in
            prev_hash @ refs_hash |> Hashing.hash
        end

    (* TODO check if equality is used *)
    fun eq (Proposal (_, h1), Proposal (_, h2)) = h1 = h2
      | eq (NonProposal (_, _, _, h1), NonProposal (_, _, _, h2)) = h1 = h2
      | eq (_, _) = false

    (* fun typ (Msg (t, _, _, _, _)) = t *)

    fun is_proposal (Proposal _) = true
      | is_proposal _ = false

    (* TODO this should be raw? *)
    fun mk_non_proposal (sender, prev_msg, recent_msgs) =
        let val hash = Word.fromInt 42 in
            NonProposal (sender, recent_msgs, prev_msg, hash)
        end
    
    fun sender (NonProposal (sender, _, _, _)) = sender
      | sender _ = raise Fail "sender not defined"

    fun get_bal_val (Proposal (b, _)) =
        let val v = Ballot.value b in SOME (b, v) end
      | get_bal_val _ = NONE
    
    fun get_prev (Proposal _) = NONE
      | get_prev (NonProposal (_, _ , prev, _)) = prev

    fun get_refs (Proposal _) = []
      | get_refs (NonProposal (_, refs , _, _)) = refs
end

functor MessageOrdKey (Msg : HPAXOS_MESSAGE) : ORD_KEY =
struct
    type ord_key = Msg.t
    fun compare (m1, m2) = Word.compare (Msg.hash m1, Msg.hash m2)
end

functor MessageUtil (Msg : HPAXOS_MESSAGE) =
struct
    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))

    type msg = Msg.t
    type ballot = Msg.Ballot.t

    (* fun does_reference_1a m : bool = *)
    (*     isSome (List.find Msg.is_one_a (Msg.get_refs m)) *)

    fun references_exactly_one_1a m : bool =
        let fun check (x, (found, false)) = (found, false)
              | check (x, (false, true)) =
                if Msg.is_proposal x then (true, true) else (false, true)
              | check (x, (true, true)) =
                if Msg.is_proposal x then (true, false) else (true, true)
        in
            case foldl check (false, true) (Msg.get_refs m) of
                (found, no_second) => found andalso no_second
        end

    fun refs_nondup m : bool =
        let
            fun check_unique (_, (refs, false)) = (refs, false)
              | check_unique (x, (refs, true)) =
                if MsgSet.member (refs, x) then
                    (refs, false)
                else
                    (MsgSet.add (refs, x), true)
        in
            #2 (foldl check_unique (MsgSet.empty, true) (Msg.get_refs m))
        end

    (* checks if m2 is in transitive closure of prev for m1 *)
    structure PrevTran :>
              sig
                  val is_prev_reachable : msg * msg -> bool
                  val is_prev_reachable' : (msg -> ballot) -> msg * msg -> bool
              end =
    struct
    fun is_prev_reachable_aux cont (m1, m2) =
        let fun loop NONE = false
              | loop (SOME m) =
                cont m andalso (Msg.eq (m, m2) orelse loop (Msg.get_prev m))
        in
            loop (SOME m1)
        end

    fun is_prev_reachable (x, y) =
        is_prev_reachable_aux (fn z => true) (x, y)

    fun is_prev_reachable' bal (x, y) =
        let
            val y_bal = bal y
            fun cont z =
                case Msg.Ballot.compare (bal z, y_bal) of
                    LESS => false
                  | _ => true
        in
            is_prev_reachable_aux cont (x, y)
        end
    end (* PrevTran *)

    (* compute transitive references of the message *)
    fun tran (pred : msg -> bool) (cont : msg -> bool) (m : msg) =
        let
            fun loop accu visited [] = accu
              | loop accu visited (x :: tl) =
                if MsgSet.member (visited, x) then
                    loop accu visited tl
                else
                    let val visited' = MsgSet.add (visited, x) in
                        if cont x then
                            let
                                val accu' = if pred x then x :: accu else accu
                                val queue' = (Msg.get_refs x) @ tl
                            in
                                loop accu' visited' queue'
                            end
                        else
                            loop accu visited' tl
                    end
        in
            loop [] MsgSet.empty [m]
        end
end
