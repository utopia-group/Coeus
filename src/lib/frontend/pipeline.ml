open Core

let default_pipeline =
  [ ExprSimplifier.run
  ; LocalDeadCodeEliminator.run
  ; DeadLocalEliminator.run
  ; ForStmtConverter.run
  ; ProcInliner.run
  ; DeadDeclEliminator.run
  ; ExprSimplifier.run
  ; EntryDuplicator.run
  ; VariableRenamer.run ]

let get_default_transforms (config : FrontendConfig.t) =
  let transform_pipeline = default_pipeline in
  let transform_pipeline =
    if config.simplify_arith then
      ExprSimplifier.run :: NonlinearArithEliminator.run :: transform_pipeline
    else transform_pipeline
  in
  let transform_pipeline =
    if config.fix_type then TypeFixer.run :: transform_pipeline
    else transform_pipeline
  in
  let transform_pipeline =
    if config.lift_assume then
      let dropped =
        List.take transform_pipeline (List.length transform_pipeline - 1)
      in
      List.append dropped [AssumeLifter.run; VariableRenamer.run]
    else transform_pipeline
  in
  let transform_pipeline =
    if config.print then List.append transform_pipeline [PrettyPrinter.run]
    else transform_pipeline
  in
  transform_pipeline

let run_transforms pipeline prog =
  let rec run_impl acc = function
    | [] -> Result.Ok acc
    | transform :: rest -> (
      match transform acc with
      | Result.Error msg -> Result.Error msg
      | Result.Ok acc' -> run_impl acc' rest )
  in
  run_impl prog pipeline

let run_default config ast =
  let transforms = get_default_transforms config in
  let open Result in
  run_transforms transforms ast
  >>= fun ast' ->
  Resolver.run ast'
  >>= fun env -> TypeChecker.check_prog env ast' >>= fun east -> Ok east
