%{
open Core

%}

%token
  T_SWAP T_CONTRA T_SEQ T_AUTOSEQ T_BLASTSEQ T_SYNC T_PSYNC
  T_SYNCIF T_SYNCCALL T_UNROLL T_COMMUTE T_COMMUTE1 T_CONCAT T_FUSE T_LICMUP
  T_RSYNCIF T_SFUSE T_LOOPELIM1
  T_EXTEND T_INTERCHANGE T_REVERSE T_PEEL T_LOOPTOREC T_INLINE
  
%token T_LEFT T_RIGHT T_NEITHER T_SEP T_PSEP T_EOF
%token <int> T_NUMBER

%start <Rule.t> main

%%

main: rule T_EOF  { $1 }

rule:
  | T_SWAP  { SwapRule.swap }
  | T_CONTRA  { ContraRule.contra }
  | T_AUTOSEQ  { AutoseqTactic.autoseq }
  | T_BLASTSEQ  { BlastseqTactic.blastseq }
  | T_SEQ T_SEP T_LEFT  { SeqRule.seq_l }
  | T_SEQ T_SEP T_RIGHT  { SeqRule.seq_r }
  | T_SYNC { SyncRule.sync }
  | T_SYNC T_SEP T_LEFT { SyncRule.sync_l }
  | T_SYNC T_SEP T_RIGHT { SyncRule.sync_r }
  | T_SYNC T_SEP T_NEITHER { SyncRule.sync_n }
  | T_PSYNC T_SEP T_LEFT  { PartialSyncRule.psync_l }
  | T_PSYNC T_SEP T_RIGHT  { PartialSyncRule.psync_r }
  | T_SYNCIF  { SyncIfRule.syncif }
  | T_RSYNCIF  { SyncIfRule.rsyncif }
  | T_SYNCCALL  { SyncCallRule.synccall }
  | T_UNROLL T_SEP T_LEFT { UnrollRule.unroll_l }
  | T_UNROLL T_SEP T_RIGHT { UnrollRule.unroll_r }
  | T_CONCAT T_SEP T_LEFT { ConcatRule.concat_l }
  | T_CONCAT T_SEP T_RIGHT { ConcatRule.concat_r }
  | T_FUSE T_SEP T_LEFT { FuseRule.fuse_l }
  | T_FUSE T_SEP T_RIGHT { FuseRule.fuse_r }
  | T_SFUSE T_SEP T_LEFT { StrongFuseRule.sfuse_l }
  | T_SFUSE T_SEP T_RIGHT { StrongFuseRule.sfuse_r }
  | T_LICMUP T_SEP T_LEFT { LicmUpRule.licmup_l }
  | T_LICMUP T_SEP T_RIGHT { LicmUpRule.licmup_r }
  | T_PEEL T_SEP T_LEFT { PeelRule.peel_l }
  | T_PEEL T_SEP T_RIGHT { PeelRule.peel_r }
  | T_EXTEND { ExtendRule.extend }
  | T_INTERCHANGE T_SEP T_LEFT { InterchangeRule.interchange_l }
  | T_INTERCHANGE T_SEP T_RIGHT { InterchangeRule.interchange_r }
  | T_REVERSE T_SEP T_LEFT { ReverseRule.reverse_l }
  | T_REVERSE T_SEP T_RIGHT { ReverseRule.reverse_r }
  | T_COMMUTE T_SEP T_LEFT { CommuteRule.commute_l }
  | T_COMMUTE T_SEP T_RIGHT { CommuteRule.commute_r }
  | T_COMMUTE1 T_SEP T_LEFT { CommuteRule.commute1_l }
  | T_COMMUTE1 T_SEP T_RIGHT { CommuteRule.commute1_r }
  | T_LOOPELIM1 T_SEP T_LEFT { LoopElimRule.loopelim1_l }
  | T_LOOPELIM1 T_SEP T_RIGHT { LoopElimRule.loopelim1_r }
  | T_INLINE T_SEP T_LEFT { InlineRule.inline_l }
  | T_INLINE T_SEP T_RIGHT { InlineRule.inline_r }
  | T_LOOPTOREC T_SEP T_LEFT { LoopToRecRule.looptorec_l }
  | T_LOOPTOREC T_SEP T_RIGHT { LoopToRecRule.looptorec_r }