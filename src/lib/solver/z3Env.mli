type t = {ctx: Z3.context; config: SolverConfig.t}

val create : SolverConfig.t -> t

val mk_solver : t -> Z3.Solver.solver

val mk_spacer : t -> Z3.Fixedpoint.fixedpoint
