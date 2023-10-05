use "hpaxos-message.sml";
use "learner-graph.sml";
use "util.sml";

signature HPAXOS_NODE =
sig
    type t
    type node_id
    val hpaxos_node : node_id -> t
end

functor HPaxos (structure Msg : HPAXOS_MESSAGE) :> HPAXOS_NODE =
struct
    type msg = Msg.t

    type acceptor_id = word
    type ballot = Msg.Ballot.t
    type value = Msg.Value.t

    structure MsgSet : ORD_SET = RedBlackSetFn (MessageOrdKey (Msg))
    structure MsgMap : ORD_MAP = RedBlackMapFn (MessageOrdKey (Msg))
    structure LearnerAcceptorMap : ORD_MAP =
        RedBlackMapFn (
            ProdLexOrdKey
                (LearnerOrdKey (Msg.Learner))
                (AcceptorOrdKey (Msg.Acceptor)))

    (* algorithm state *)
    datatype known_msgs = KnownMsgs of MsgSet.set
    datatype recent_msgs = RecentMsgs of MsgSet.set

    datatype state = State of known_msgs * recent_msgs

    (* message info state *)
    datatype info_bal_val = InfoBalVal of (ballot * value) MsgMap.map
    datatype info_W = InfoW of ((msg * msg option) LearnerAcceptorMap.map) MsgMap.map
    datatype msg_info = MsgInfo of info_bal_val * info_W

    (* memo state *)
    datatype cache = Cache of int

    datatype acceptor = Acc of acceptor_id * state * msg_info * cache

    type t = acceptor
    type node_id = acceptor_id

    fun is_known (m : msg) (State (KnownMsgs k, r)) : bool =
        MsgSet.member (k, m)

    fun add_known (m : msg) (State (KnownMsgs k, r)) : state =
        State (KnownMsgs (MsgSet.add (k, m)), r)

    fun add_recent (m : msg) (State (k, RecentMsgs r)) : state =
        State (k, RecentMsgs (MsgSet.add (r, m)))

    fun compute_bal_val (m : msg) (InfoBalVal info) : ballot * value =
        if Msg.is_one_a m then
            Option.valOf (Msg.get_bal_val m)
        else
            let
                fun helper (x, (max_bal, max_val)) =
                    let val (b, v) = MsgMap.lookup (info, x) in
                        case Msg.Ballot.compare (b, max_bal) of
                            LESS => (max_bal, max_val)
                          | _ => (b, v)
                    end
                val refs = Msg.get_refs m (* refs is non-empty since m is not 1a *)
            in
                List.foldr helper (Msg.Ballot.zero, Msg.Value.default) refs
            end

    fun compute_W (m : msg) (InfoBalVal info_bal_val) (InfoW info_w) : (msg * msg option) LearnerAcceptorMap.map =
        let
            fun pick_best_two_from_list (ms : msg list) : (msg * msg option) option =
                let fun picker (x, NONE) = SOME (x, NONE)
                      | picker (x, SOME (best1, o_best2)) =
                        let
                            val (b, v) = MsgMap.lookup (info_bal_val, x)
                            val (bal1, val1) = MsgMap.lookup (info_bal_val, best1)
                            fun pick_second_best
                                    fst_best_val candidate (candidate_bal, candidate_val) cur_snd_best_option =
                                if Msg.Value.eq (candidate_val, fst_best_val) then
                                    cur_snd_best_option
                                else
                                    let val new_snd_best =
                                            case cur_snd_best_option of
                                                NONE => candidate
                                              | SOME cur_snd_best =>
                                                let val (bal2, _) = MsgMap.lookup (info_bal_val, cur_snd_best) in
                                                    case Msg.Ballot.compare (candidate_bal, bal2) of
                                                        GREATER => candidate
                                                      | _ => cur_snd_best
                                                end
                                    in
                                        SOME new_snd_best
                                    end
                            val new_best =
                                case Msg.Ballot.compare (b, bal1) of
                                    GREATER => (x, pick_second_best v best1 (bal1, val1) o_best2)
                                  | _ => (best1, pick_second_best val1 x (b, v) o_best2)
                        in
                            SOME new_best
                        end
                in
                    List.foldr picker NONE ms
                end
            fun pick_best_two (a : msg * msg option, b : msg * msg option) =
                let fun to_list (best1, NONE) = [best1]
                      | to_list (best1, SOME best2) = [best1, best2]
                in
                    Option.valOf (pick_best_two_from_list (List.concat [to_list a, to_list b]))
                end
            fun helper (r, w) =
                let val r_info_w = MsgMap.lookup (info_w, r)
                in
                    LearnerAcceptorMap.unionWith pick_best_two (r_info_w, w)
                end
            val refs = Msg.get_refs m
            val w0 =
                if Msg.is_two_a m then
                    let
                        val lrn = Option.valOf (Msg.learner m)
                        val acc = Msg.sender m
                    in
                        LearnerAcceptorMap.insert
                            (LearnerAcceptorMap.empty, (lrn, acc), (m, NONE))
                    end
                else
                    LearnerAcceptorMap.empty
        in
            List.foldr helper w0 refs
        end

    datatype AcceptorStatus = Caught
                            | Uncaught of msg

    fun join (Uncaught m1, Uncaught m2) =
        Uncaught m1
      | join (_, _) = Caught

    (* fun compute_and_store_bal_val (m : msg) info : msg_info = *)
    (*     let *)
    (*         val bv = compute_bal_val m info *)
    (*         val MsgInfo (info) = info *)
    (*     in *)
    (*         MsgInfo (MsgMap.insert (info, m, InfoBalVal(bv))) *)
    (*     end *)

    fun hpaxos_node (id : node_id) : t =
        Acc (id,
             State (KnownMsgs MsgSet.empty,
                    RecentMsgs MsgSet.empty),
             MsgInfo (InfoBalVal MsgMap.empty, InfoW MsgMap.empty),
             Cache 0)
end
