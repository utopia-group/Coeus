open Core
open Ast.Coeus

let plus_one e =
  let open Expr in
  match simplify e with
  | LiteralExpr (Literal.IntLit i) ->
      LiteralExpr (Literal.IntLit Bigint.(i + one))
  | UnaryExpr (UnaryOperator.Neg, LiteralExpr (Literal.IntLit i)) ->
      LiteralExpr (Literal.IntLit Bigint.(one - i))
  | UnaryExpr (UnaryOperator.Neg, e) ->
      BinaryExpr
        (BinaryOperator.Minus, LiteralExpr (Literal.IntLit Bigint.one), e)
  | BinaryExpr (BinaryOperator.Plus, LiteralExpr (Literal.IntLit i), rhs) ->
      BinaryExpr
        ( BinaryOperator.Plus
        , LiteralExpr (Literal.IntLit Bigint.(i + one))
        , rhs )
  | BinaryExpr (BinaryOperator.Plus, lhs, LiteralExpr (Literal.IntLit i)) ->
      BinaryExpr
        ( BinaryOperator.Plus
        , lhs
        , LiteralExpr (Literal.IntLit Bigint.(i + one)) )
  | BinaryExpr (BinaryOperator.Minus, LiteralExpr (Literal.IntLit i), rhs) ->
      BinaryExpr
        ( BinaryOperator.Minus
        , LiteralExpr (Literal.IntLit Bigint.(i + one))
        , rhs )
  | BinaryExpr (BinaryOperator.Minus, lhs, LiteralExpr (Literal.IntLit i)) ->
      BinaryExpr
        ( BinaryOperator.Minus
        , lhs
        , LiteralExpr (Literal.IntLit Bigint.(i - one)) )
  | _ ->
      BinaryExpr
        (BinaryOperator.Plus, e, LiteralExpr (Literal.IntLit Bigint.one))

let minus_one e =
  let open Expr in
  match simplify e with
  | LiteralExpr (Literal.IntLit i) ->
      LiteralExpr (Literal.IntLit Bigint.(i - one))
  | UnaryExpr (UnaryOperator.Neg, LiteralExpr (Literal.IntLit i)) ->
      LiteralExpr (Literal.IntLit Bigint.(-(one + i)))
  | UnaryExpr (UnaryOperator.Neg, e) ->
      BinaryExpr
        (BinaryOperator.Minus, LiteralExpr (Literal.IntLit Bigint.(-one)), e)
  | BinaryExpr (BinaryOperator.Plus, LiteralExpr (Literal.IntLit i), rhs) ->
      BinaryExpr
        ( BinaryOperator.Plus
        , LiteralExpr (Literal.IntLit Bigint.(i - one))
        , rhs )
  | BinaryExpr (BinaryOperator.Plus, lhs, LiteralExpr (Literal.IntLit i)) ->
      BinaryExpr
        ( BinaryOperator.Plus
        , lhs
        , LiteralExpr (Literal.IntLit Bigint.(i - one)) )
  | BinaryExpr (BinaryOperator.Minus, LiteralExpr (Literal.IntLit i), rhs) ->
      BinaryExpr
        ( BinaryOperator.Minus
        , LiteralExpr (Literal.IntLit Bigint.(i - one))
        , rhs )
  | BinaryExpr (BinaryOperator.Minus, lhs, LiteralExpr (Literal.IntLit i)) ->
      BinaryExpr
        ( BinaryOperator.Minus
        , lhs
        , LiteralExpr (Literal.IntLit Bigint.(i + one)) )
  | _ ->
      BinaryExpr
        (BinaryOperator.Plus, e, LiteralExpr (Literal.IntLit Bigint.one))

let rec normalize_cond expr =
  let open Expr in
  match expr with
  | LiteralExpr _ | VarExpr _ -> expr
  | UnaryExpr (op, e) -> UnaryExpr (op, normalize_cond e)
  | ArraySelectExpr {base; indices} ->
      ArraySelectExpr
        {base= normalize_cond base; indices= List.map indices ~f:normalize_cond}
  | ArrayStoreExpr {base; indices; value} ->
      ArrayStoreExpr
        { base= normalize_cond base
        ; indices= List.map indices ~f:normalize_cond
        ; value= normalize_cond value }
  | AnnotatedExpr {annot; expr} ->
      AnnotatedExpr {annot; expr= normalize_cond expr}
  | CondExpr {cond; true_val; false_val} ->
      CondExpr
        { cond= normalize_cond cond
        ; true_val= normalize_cond true_val
        ; false_val= normalize_cond false_val }
  | FunCallExpr {name; args} ->
      FunCallExpr {name; args= List.map args ~f:normalize_cond}
  | QuantifiedExpr {quantifier; bindings; body} ->
      QuantifiedExpr {quantifier; bindings; body= normalize_cond body}
  | BinaryExpr (op, lhs, rhs) ->
    match op with
    | BinaryOperator.Le -> BinaryExpr (BinaryOperator.Lt, lhs, plus_one rhs)
    | BinaryOperator.Ge -> BinaryExpr (BinaryOperator.Gt, lhs, minus_one rhs)
    | _ -> BinaryExpr (op, normalize_cond lhs, normalize_cond rhs)

let run_on_proc proc =
  let stmts = Stmts.map_expr ~f:normalize_cond proc.Procedure.stmts in
  {proc with stmts}

let run prog =
  Logs.debug (fun m -> m "ConditionNormalizer starts") ;
  let prog = proc_map prog ~f:run_on_proc in
  Logs.debug (fun m -> m "ConditionNormalizer successfully finished") ;
  Result.Ok prog
