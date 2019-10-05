open Core

let is_applicable _ _ = true

let apply _ _ goal = Some (Goal.swap_stmts goal)

let swap = LocalRule.mk_rule ~is_applicable ~name:"swap" apply
