open Core
open Ast.Coeus
open Prover

let pick_rule config state =
  if BlastseqTactic.blastseq.is_applicable config state then
    Some BlastseqTactic.blastseq
  else Some SeqRule.seq_l

let create () = RulePicker.to_linear_strategy pick_rule
