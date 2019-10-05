open Core

type t =
  { curr_depth: int
  ; goals_remaining: int
  ; ast_size: int
  ; left_stmt_type: int option
  ; left_stmt1_type: int option
  ; left_stmt2_type: int option
  ; right_stmt_type: int option
  ; right_stmt1_type: int option
  ; right_stmt2_type: int option
  ; left_stmt_difficulty: int option
  ; right_stmt_difficulty: int option
  ; left_while_complex_cond: bool option
  ; right_while_complex_cond: bool option
  ; left_for_count: int option
  ; right_for_count: int option
  ; stmt_structural_same: bool
  ; for_ubound_ratio: float option
  ; for_lbound_ratio: float option
  ; for_step_ratio: float option
  ; for_flhint: bool option
  ; for_fuhint: bool option
  ; for_blhint: bool option
  ; for_buhint: bool option
  ; is_autoseq_app: bool
  ; is_blastseq_app: bool
  ; is_extend_app: bool
  ; is_syncif_app: bool
  ; is_peell_app: bool
  ; is_peelr_app: bool
  ; is_reversel_app: bool
  ; is_reverser_app: bool
  ; is_commutel_app: bool
  ; is_commuter_app: bool
  ; is_commute1l_app: bool
  ; is_commute1r_app: bool
  ; is_loopelim1l_app: bool
  ; is_loopelim1r_app: bool
  ; is_fusel_app: bool
  ; is_fuser_app: bool
  ; is_sfusel_app: bool
  ; is_sfuser_app: bool
  ; is_looptorecl_app: bool
  ; is_looptorecr_app: bool
  ; is_synccall_app: bool
  ; is_inlinel_app: bool
  ; is_inliner_app: bool
  ; is_concatl_app: bool
  ; is_concatr_app: bool }
[@@deriving sexp, fields]

let to_feature_vec (prover_config : ProverConfig.t) encoding =
  let conv (type a) (to_floats : a -> float list) field : float list =
    let field_value = Field.get field encoding in
    to_floats field_value
  in
  let int_to_float = Int.to_float in
  let bool_to_float = function false -> 0.0 | true -> 1.0 in
  let float_to_floats f = [f] in
  let bool_to_floats = Fn.compose float_to_floats bool_to_float in
  let opt_to_floats v_to_float = function
    | None -> [0.0; 0.0]
    | Some v -> [1.0; v_to_float v]
  in
  let int_bound_to_float ~lower ~upper i =
    let f = int_to_float i in
    let lower = int_to_float lower in
    let upper = int_to_float upper in
    let nf = (f -. lower) /. (upper -. lower) in
    if Float.is_inf nf || Float.is_nan nf then 0.0 else nf
  in
  let int_squash_to_float i =
    let r = Float.tanh (int_to_float i) in
    if Float.is_inf r || Float.is_nan r then 0.0 else r
  in
  let conv_int_bound ~lower ~upper =
    conv (Fn.compose float_to_floats (int_bound_to_float ~lower ~upper))
  in
  let conv_int_squash =
    conv (Fn.compose float_to_floats int_squash_to_float)
  in
  let conv_bool = conv bool_to_floats in
  let conv_opt_float = conv (opt_to_floats Fn.id) in
  let conv_opt_int_bound ~lower ~upper =
    conv (opt_to_floats (int_bound_to_float ~lower ~upper))
  in
  let conv_opt_int_stmt_type = conv_opt_int_bound ~lower:1 ~upper:6 in
  let conv_opt_bool = conv (opt_to_floats bool_to_float) in
  let float_lists =
    Fields.to_list
      ~curr_depth:
        ( match prover_config.depth_limit with
        | Some upper -> conv_int_bound ~lower:0 ~upper
        | None -> conv_int_squash )
      ~goals_remaining:conv_int_squash ~ast_size:conv_int_squash
      ~left_stmt_type:conv_opt_int_stmt_type
      ~left_stmt1_type:conv_opt_int_stmt_type
      ~left_stmt2_type:conv_opt_int_stmt_type
      ~right_stmt_type:conv_opt_int_stmt_type
      ~right_stmt1_type:conv_opt_int_stmt_type
      ~right_stmt2_type:conv_opt_int_stmt_type
      ~left_stmt_difficulty:(conv_opt_int_bound ~lower:0 ~upper:1000)
      ~right_stmt_difficulty:(conv_opt_int_bound ~lower:0 ~upper:1000)
      ~left_while_complex_cond:conv_opt_bool
      ~right_while_complex_cond:conv_opt_bool
      ~left_for_count:(conv_opt_int_bound ~lower:0 ~upper:1000)
      ~right_for_count:(conv_opt_int_bound ~lower:0 ~upper:1000)
      ~stmt_structural_same:conv_bool ~for_ubound_ratio:conv_opt_float
      ~for_lbound_ratio:conv_opt_float ~for_step_ratio:conv_opt_float
      ~for_flhint:conv_opt_bool ~for_fuhint:conv_opt_bool
      ~for_blhint:conv_opt_bool ~for_buhint:conv_opt_bool
      ~is_autoseq_app:conv_bool ~is_blastseq_app:conv_bool
      ~is_extend_app:conv_bool ~is_syncif_app:conv_bool ~is_peell_app:conv_bool
      ~is_peelr_app:conv_bool ~is_reversel_app:conv_bool
      ~is_reverser_app:conv_bool ~is_commutel_app:conv_bool
      ~is_commuter_app:conv_bool ~is_commute1l_app:conv_bool
      ~is_commute1r_app:conv_bool ~is_loopelim1l_app:conv_bool
      ~is_loopelim1r_app:conv_bool ~is_fusel_app:conv_bool
      ~is_fuser_app:conv_bool ~is_sfusel_app:conv_bool ~is_sfuser_app:conv_bool
      ~is_looptorecl_app:conv_bool ~is_looptorecr_app:conv_bool
      ~is_synccall_app:conv_bool ~is_inlinel_app:conv_bool
      ~is_inliner_app:conv_bool ~is_concatl_app:conv_bool
      ~is_concatr_app:conv_bool
  in
  List.concat float_lists
