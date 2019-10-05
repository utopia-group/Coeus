open Core

let candidate_rules =
  [| SeqRule.seq_l
   ; SeqRule.seq_r
   ; AutoseqTactic.autoseq
   ; BlastseqTactic.blastseq
   ; ExtendRule.extend
   ; SyncIfRule.syncif
   ; SyncIfRule.rsyncif
   ; PeelRule.peel_l
   ; PeelRule.peel_r
   ; ReverseRule.reverse_l
   ; ReverseRule.reverse_r
   ; UnrollRule.unroll_l
   ; UnrollRule.unroll_r
   ; CommuteRule.commute_l
   ; CommuteRule.commute_r
   ; CommuteRule.commute1_l
   ; CommuteRule.commute1_r
   ; LoopElimRule.loopelim1_l
   ; LoopElimRule.loopelim1_r
   ; FuseRule.fuse_l
   ; FuseRule.fuse_r
   ; StrongFuseRule.sfuse_l
   ; StrongFuseRule.sfuse_r
   ; ConcatRule.concat_l
   ; ConcatRule.concat_r
   ; SyncRule.sync
   ; SyncRule.sync_l
   ; SyncRule.sync_r
   ; SyncRule.sync_n
   ; PartialSyncRule.psync_l
   ; PartialSyncRule.psync_r
   ; LoopToRecRule.looptorec_l
   ; LoopToRecRule.looptorec_r
   ; SyncCallRule.synccall
   ; InlineRule.inline_l
   ; InlineRule.inline_r |]

let candidate_rule_names = Array.map candidate_rules ~f:(fun r -> r.Rule.name)

let candidate_rule_index_table =
  let num_rules = Array.length candidate_rules in
  let name_table = String.Table.create ~size:num_rules () in
  Array.iteri candidate_rule_names ~f:(fun id s ->
      match String.Table.add name_table ~key:s ~data:id with
      | `Duplicate ->
          let msg =
            Fmt.strf "[INTERNAL] Duplicate entry in candidate rules: %s" s
          in
          failwith msg
      | `Ok -> () ) ;
  name_table

let index_of r = String.Table.find candidate_rule_index_table r.Rule.name

let index_of_exn r =
  match index_of r with
  | Some i -> i
  | None ->
      let msg = Fmt.strf "Cannot find rule in candidate set: %s" r.Rule.name in
      raise (Invalid_argument msg)

(* HACK: we rely on regex to perform the conflict matching. All regex libraries I can find out there only work for strings and none of them allow the user to specify a custom alphabet. There are some really annoying pitfalls when working with character-based regexes (special chars, escaping, etc.). Luckily, the number of rules in our experiments is small, so we are able to assign a "safe" ASCII character to each of the rule. This approach obvious won't scale if the number of rules is larger, but that's a concern for another day. *)
let letters =
  String.to_array
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let char_of r = Option.map (index_of r) ~f:(Array.get letters)

let char_of_exn r =
  match char_of r with
  | Some i -> i
  | None ->
      let msg = Fmt.strf "Cannot find char for rule: %s" r.Rule.name in
      raise (Invalid_argument msg)
