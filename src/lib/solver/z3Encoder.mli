open Ast.Ecoeus

val encode_type : Z3Env.t -> Type.t -> Z3.Sort.sort

val encode_literal : Z3Env.t -> Literal.t -> Z3.Expr.expr

val encode_var : Z3Env.t -> VarBinding.t -> Z3.Expr.expr

val encode_fundecl : Z3Env.t -> FunDecl.t -> Z3.FuncDecl.func_decl

val encode_expr : Z3Env.t -> Expr.t -> Z3.Expr.expr
