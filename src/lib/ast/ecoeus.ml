open Core
module Identifier = Coeus.Identifier
module Type = Coeus.Type
module Literal = Coeus.Literal
module Quantifier = Coeus.Quantifier
module UnaryOperator = Coeus.UnaryOperator
module BinaryOperator = Coeus.BinaryOperator
module Side = Coeus.Side
module VarBinding = Coeus.VarBinding
module FunDecl = Coeus.FunDecl
module EntrySpec = Coeus.EntrySpec

let pp_comma fmt () = Fmt.string fmt ", "

module ExprInfo = struct
  type t = {ast_size: int; ty: Type.t} [@@deriving sexp, compare, hash]

  let pp fmt {ast_size; ty} =
    Fmt.pf fmt "ExprInfo{ ast_size = %d; ty = %a }" ast_size Type.pp ty
end

module Expr = struct
  module T = struct
    type t = {bare_expr: u; info: ExprInfo.t} [@@deriving sexp, compare, hash]

    and u =
      | LiteralExpr of Literal.t
      | VarExpr of VarBinding.t
      | UnaryExpr of UnaryOperator.t * t
      | BinaryExpr of BinaryOperator.t * t * t
      | ArraySelectExpr of {base: t; indices: t list}
      | ArrayStoreExpr of {base: t; indices: t list; value: t}
      | CondExpr of {cond: t; true_val: t; false_val: t}
      | FunCallExpr of {func: FunDecl.t; args: t list}
      | QuantifiedExpr of
          { quantifier: Quantifier.t
          ; bindings: VarBinding.t list
          ; body: t }
    [@@deriving sexp, compare, hash]

    let type_of {info= ExprInfo.({ty; _}); _} = ty

    let ast_size_of {info= ExprInfo.({ast_size; _}); _} = ast_size

    let ast_size_of_exprs =
      List.fold ~init:0 ~f:(fun acc e -> acc + ast_size_of e)

    let mk_literal lit =
      let ty = Literal.type_of lit in
      let ast_size = 1 in
      {bare_expr= LiteralExpr lit; info= ExprInfo.{ast_size; ty}}

    let mk_var binding =
      let ast_size = 1 in
      {bare_expr= VarExpr binding; info= ExprInfo.{ast_size; ty= binding.ty}}

    let mk_unary op e =
      let ast_size = 1 + ast_size_of e in
      { bare_expr= UnaryExpr (op, e)
      ; info= ExprInfo.{ast_size; ty= UnaryOperator.ret_type_of op} }

    let mk_binary op lhs rhs =
      let ast_size = 1 + ast_size_of lhs + ast_size_of rhs in
      { bare_expr= BinaryExpr (op, lhs, rhs)
      ; info= ExprInfo.{ast_size; ty= BinaryOperator.ret_type_of op} }

    let mk_array_select base indices ty =
      let ast_size = 1 + ast_size_of base + ast_size_of_exprs indices in
      { bare_expr= ArraySelectExpr {base; indices}
      ; info= ExprInfo.{ast_size; ty} }

    let mk_array_store base indices value =
      let ast_size =
        1 + ast_size_of base + ast_size_of_exprs indices + ast_size_of value
      in
      { bare_expr= ArrayStoreExpr {base; indices; value}
      ; info= ExprInfo.{ast_size; ty= type_of base} }

    let mk_cond cond true_val false_val =
      let ast_size =
        1 + ast_size_of cond + ast_size_of true_val + ast_size_of false_val
      in
      { bare_expr= CondExpr {cond; true_val; false_val}
      ; info= ExprInfo.{ast_size; ty= type_of true_val} }

    let mk_funcall func args =
      let ast_size = 1 + ast_size_of_exprs args in
      { bare_expr= FunCallExpr {func; args}
      ; info= ExprInfo.{ast_size; ty= func.ret_ty} }

    let mk_quantified quantifier bindings body =
      let ast_size = 1 + List.length bindings + ast_size_of body in
      { bare_expr= QuantifiedExpr {quantifier; bindings; body}
      ; info= ExprInfo.{ast_size; ty= Type.BoolType} }

    let logical_negate_of expr =
      match expr.bare_expr with
      | LiteralExpr (Literal.BoolLit true) ->
          mk_literal (Literal.BoolLit false)
      | LiteralExpr (Literal.BoolLit false) ->
          mk_literal (Literal.BoolLit true)
      | UnaryExpr (UnaryOperator.Not, e) -> e
      | BinaryExpr (BinaryOperator.Le, lhs, rhs) ->
          mk_binary BinaryOperator.Gt lhs rhs
      | BinaryExpr (BinaryOperator.Lt, lhs, rhs) ->
          mk_binary BinaryOperator.Ge lhs rhs
      | BinaryExpr (BinaryOperator.Ge, lhs, rhs) ->
          mk_binary BinaryOperator.Lt lhs rhs
      | BinaryExpr (BinaryOperator.Gt, lhs, rhs) ->
          mk_binary BinaryOperator.Le lhs rhs
      | BinaryExpr (BinaryOperator.Eq, lhs, rhs) ->
          mk_binary BinaryOperator.Ne lhs rhs
      | BinaryExpr (BinaryOperator.Ne, lhs, rhs) ->
          mk_binary BinaryOperator.Eq lhs rhs
      | _ -> mk_unary UnaryOperator.Not expr

    let conjunct_exprs = function
      | [] -> mk_literal (Literal.BoolLit true)
      | [e] -> e
      | es ->
          List.reduce_exn es ~f:(fun e0 e1 ->
              mk_binary BinaryOperator.And e0 e1 )

    let subst ~f expr =
      let rec run_expr f expr =
        match expr.bare_expr with
        | LiteralExpr _ -> (false, expr)
        | VarExpr v -> (
          match f v.name with None -> (false, expr) | Some e -> (true, e) )
        | UnaryExpr (op, e) ->
            let changed, e' = run_expr f e in
            if changed then (true, mk_unary op e') else (false, expr)
        | BinaryExpr (op, lhs, rhs) ->
            let lchanged, lhs' = run_expr f lhs in
            let rchanged, rhs' = run_expr f rhs in
            if lchanged || rchanged then (true, mk_binary op lhs' rhs')
            else (false, expr)
        | ArraySelectExpr {base; indices} ->
            let bchanged, base' = run_expr f base in
            let ichanged, indices' = run_exprs f indices in
            if bchanged || ichanged then
              (true, mk_array_select base' indices' (type_of expr))
            else (false, expr)
        | ArrayStoreExpr {base; indices; value} ->
            let bchanged, base' = run_expr f base in
            let ichanged, indices' = run_exprs f indices in
            let vchanged, value' = run_expr f value in
            if bchanged || ichanged || vchanged then
              (true, mk_array_store base' indices' value')
            else (false, expr)
        | CondExpr {cond; true_val; false_val} ->
            let cchanged, cond' = run_expr f cond in
            let tchanged, true_val' = run_expr f true_val in
            let fchanged, false_val' = run_expr f false_val in
            if cchanged || tchanged || fchanged then
              (true, mk_cond cond' true_val' false_val')
            else (false, expr)
        | FunCallExpr {func; args} ->
            let changed, args' = run_exprs f args in
            if changed then (true, mk_funcall func args') else (false, expr)
        | QuantifiedExpr {quantifier; bindings; body} ->
            let f' v =
              if
                Option.is_some
                  (List.find bindings ~f:(fun VarBinding.({name; _}) ->
                       Identifier.equal name v ))
              then None
              else f v
            in
            let changed, body' = run_expr f' body in
            if changed then (true, mk_quantified quantifier bindings body')
            else (false, expr)
      and run_exprs subst_map exprs =
        let changed, rev_exprs' =
          List.fold exprs ~init:(false, []) ~f:(fun (acc_changed, acc) expr ->
              let changed, expr' = run_expr subst_map expr in
              let acc' = expr' :: acc in
              let acc_changed' = changed || acc_changed in
              (acc_changed', acc') )
        in
        (changed, List.rev rev_exprs')
      in
      let _, expr' = run_expr f expr in
      expr'

    let map_var ~f e =
      let rec run_var env expr =
        match expr.bare_expr with
        | LiteralExpr _ -> expr
        | VarExpr vb as e ->
            let bare_expr =
              if Identifier.Set.mem env vb.name then e
              else
                let vb' = {vb with name= f vb.name} in
                VarExpr vb'
            in
            {expr with bare_expr}
        | UnaryExpr (op, e) ->
            let bare_expr = UnaryExpr (op, run_var env e) in
            {expr with bare_expr}
        | BinaryExpr (op, lhs, rhs) ->
            let bare_expr =
              BinaryExpr (op, run_var env lhs, run_var env rhs)
            in
            {expr with bare_expr}
        | ArraySelectExpr {base; indices} ->
            let bare_expr =
              ArraySelectExpr
                { base= run_var env base
                ; indices= List.map indices ~f:(run_var env) }
            in
            {expr with bare_expr}
        | ArrayStoreExpr {base; indices; value} ->
            let bare_expr =
              ArrayStoreExpr
                { base= run_var env base
                ; indices= List.map indices ~f:(run_var env)
                ; value= run_var env value }
            in
            {expr with bare_expr}
        | CondExpr {cond; true_val; false_val} ->
            let bare_expr =
              CondExpr
                { cond= run_var env cond
                ; true_val= run_var env true_val
                ; false_val= run_var env false_val }
            in
            {expr with bare_expr}
        | FunCallExpr {func; args} ->
            let bare_expr =
              FunCallExpr {func; args= List.map args ~f:(run_var env)}
            in
            {expr with bare_expr}
        | QuantifiedExpr {quantifier; bindings; body} ->
            let env =
              List.fold bindings ~init:env ~f:
                (fun acc VarBinding.({name; _}) -> Identifier.Set.add acc name
              )
            in
            let bare_expr =
              QuantifiedExpr {quantifier; bindings; body= run_var env body}
            in
            {expr with bare_expr}
      in
      run_var Identifier.Set.empty e

    let iter_var ~f e =
      let rec run_var env expr =
        match expr.bare_expr with
        | LiteralExpr _ -> ()
        | VarExpr vb -> if not (Identifier.Set.mem env vb.name) then f vb
        | UnaryExpr (_, e) -> run_var env e
        | BinaryExpr (_, lhs, rhs) -> run_var env lhs ; run_var env rhs
        | ArraySelectExpr {base; indices} ->
            run_var env base ;
            List.iter indices ~f:(run_var env)
        | ArrayStoreExpr {base; indices; value} ->
            run_var env base ;
            List.iter indices ~f:(run_var env) ;
            run_var env value
        | CondExpr {cond; true_val; false_val} ->
            run_var env cond ; run_var env true_val ; run_var env false_val
        | FunCallExpr {args; _} -> List.iter args ~f:(run_var env)
        | QuantifiedExpr {bindings; body; _} ->
            let env =
              List.fold bindings ~init:env ~f:
                (fun acc VarBinding.({name; _}) -> Identifier.Set.add acc name
              )
            in
            run_var env body
      in
      run_var Identifier.Set.empty e

    let free_var_set_of e =
      let free_vars = VarBinding.Hash_set.create ~size:16 () in
      iter_var e ~f:(fun vb -> Hash_set.add free_vars vb) ;
      free_vars

    let free_var_set_of_exprs es =
      let free_vars = VarBinding.Hash_set.create ~size:64 () in
      List.iter es ~f:(iter_var ~f:(fun vb -> Hash_set.add free_vars vb)) ;
      free_vars

    let free_vars_of e =
      let free_var_set = free_var_set_of e in
      Hash_set.to_list free_var_set

    let free_vars_of_exprs es =
      let free_var_set = free_var_set_of_exprs es in
      Hash_set.to_list free_var_set

    let rec literals_of_impl lit_set expr =
      match expr.bare_expr with
      | LiteralExpr l -> Hash_set.add lit_set l
      | VarExpr _ -> ()
      | UnaryExpr (_, e) -> literals_of_impl lit_set e
      | BinaryExpr (_, lhs, rhs) ->
          literals_of_impl lit_set lhs ;
          literals_of_impl lit_set rhs
      | ArraySelectExpr {base; indices} ->
          literals_of_impl lit_set base ;
          List.iter indices ~f:(literals_of_impl lit_set)
      | ArrayStoreExpr {base; indices; value} ->
          literals_of_impl lit_set base ;
          literals_of_impl lit_set value ;
          List.iter indices ~f:(literals_of_impl lit_set)
      | CondExpr {cond; true_val; false_val} ->
          literals_of_impl lit_set cond ;
          literals_of_impl lit_set true_val ;
          literals_of_impl lit_set false_val
      | FunCallExpr {args; _} -> List.iter args ~f:(literals_of_impl lit_set)
      | QuantifiedExpr {body; _} -> literals_of_impl lit_set body

    let literals_of expr =
      let lit_set = Literal.Hash_set.create ~size:4 () in
      literals_of_impl lit_set expr ;
      Hash_set.to_list lit_set

    let literals_of_exprs es =
      let lit_set = Literal.Hash_set.create ~size:16 () in
      List.iter es ~f:(literals_of_impl lit_set) ;
      Hash_set.to_list lit_set

    let rec to_coeus expr =
      match expr.bare_expr with
      | LiteralExpr l -> Coeus.Expr.LiteralExpr l
      | VarExpr vb -> Coeus.Expr.VarExpr vb.name
      | UnaryExpr (op, e) ->
          let e' = to_coeus e in
          Coeus.Expr.UnaryExpr (op, e')
      | BinaryExpr (op, lhs, rhs) ->
          let lhs' = to_coeus lhs in
          let rhs' = to_coeus rhs in
          Coeus.Expr.BinaryExpr (op, lhs', rhs')
      | ArraySelectExpr {base; indices} ->
          let base' = to_coeus base in
          let indices' = List.map indices ~f:to_coeus in
          Coeus.Expr.ArraySelectExpr {base= base'; indices= indices'}
      | ArrayStoreExpr {base; indices; value} ->
          let base' = to_coeus base in
          let indices' = List.map indices ~f:to_coeus in
          let value' = to_coeus value in
          Coeus.Expr.ArrayStoreExpr
            {base= base'; indices= indices'; value= value'}
      | CondExpr {cond; true_val; false_val} ->
          let cond' = to_coeus cond in
          let true_val' = to_coeus true_val in
          let false_val' = to_coeus false_val in
          Coeus.Expr.CondExpr
            {cond= cond'; true_val= true_val'; false_val= false_val'}
      | FunCallExpr {func; args} ->
          let name = func.name in
          let args' = List.map args ~f:to_coeus in
          Coeus.Expr.FunCallExpr {name; args= args'}
      | QuantifiedExpr {quantifier; bindings; body} ->
          let body' = to_coeus body in
          Coeus.Expr.QuantifiedExpr {quantifier; bindings; body= body'}

    (* We don't do as heavy computation as we did in Coeus.Expr.simplify *)

    let rec simplify expr =
      match expr.bare_expr with
      | LiteralExpr _ | VarExpr _ -> expr
      | UnaryExpr (op, e) -> simplify_unary op e
      | BinaryExpr (op, lhs, rhs) -> simplify_binary op lhs rhs
      | _ -> expr

    and simplify_unary op e =
      let open UnaryOperator in
      match (op, e.bare_expr) with
      | Not, _ -> logical_negate_of (simplify e)
      | Neg, LiteralExpr (Literal.IntLit i) ->
          mk_literal (Literal.IntLit (Bigint.neg i))
      | Neg, UnaryExpr (Neg, e) -> simplify e
      | _, _ -> mk_unary op (simplify e)

    and simplify_binary op lhs rhs =
      let open BinaryOperator in
      match (op, lhs.bare_expr, rhs.bare_expr) with
      | Plus, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.IntLit Bigint.(i + j))
      | Plus, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero -> lhs
      | ( Plus
        , BinaryExpr (Plus, e, {bare_expr= LiteralExpr (Literal.IntLit i); _})
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Plus e (mk_literal (Literal.IntLit Bigint.(i + j)))
      | ( Plus
        , BinaryExpr (Plus, {bare_expr= LiteralExpr (Literal.IntLit i); _}, e)
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Plus (mk_literal (Literal.IntLit Bigint.(i + j))) e
      | ( Plus
        , BinaryExpr (Minus, e, {bare_expr= LiteralExpr (Literal.IntLit i); _})
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Minus e (mk_literal (Literal.IntLit Bigint.(i - j)))
      | ( Plus
        , BinaryExpr (Minus, {bare_expr= LiteralExpr (Literal.IntLit i); _}, e)
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Minus (mk_literal (Literal.IntLit Bigint.(i + j))) e
      | Plus, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero -> rhs
      | Plus, _, UnaryExpr (UnaryOperator.Neg, e) ->
          simplify (mk_binary Minus lhs e)
      | Plus, BinaryExpr (Minus, e0, e1), e2
        when [%compare.equal : u] e1.bare_expr e2 ->
          simplify e0
      | Minus, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.IntLit Bigint.(i - j))
      | Minus, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero -> lhs
      | ( Minus
        , BinaryExpr (Plus, e, {bare_expr= LiteralExpr (Literal.IntLit i); _})
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Plus e (mk_literal (Literal.IntLit Bigint.(i - j)))
      | ( Minus
        , BinaryExpr (Minus, e, {bare_expr= LiteralExpr (Literal.IntLit i); _})
        , LiteralExpr (Literal.IntLit j) ) ->
          simplify_binary Minus e (mk_literal (Literal.IntLit Bigint.(i + j)))
      | Minus, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
          simplify (mk_unary UnaryOperator.Neg rhs)
      | Minus, lhs, rhs when [%compare.equal : u] lhs rhs ->
          mk_literal (Literal.IntLit Bigint.zero)
      | Minus, _, UnaryExpr (UnaryOperator.Neg, e) ->
          simplify (mk_binary Plus lhs e)
      | Minus, BinaryExpr (Plus, e0, e1), e2
        when [%compare.equal : u] e0.bare_expr e2 ->
          simplify e1
      | Minus, BinaryExpr (Plus, e0, e1), e2
        when [%compare.equal : u] e1.bare_expr e2 ->
          simplify e0
      | Mult, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.IntLit Bigint.(i * j))
      | Mult, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.zero)
      | Mult, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.zero)
      | Mult, _, LiteralExpr (Literal.IntLit i) when i = Bigint.one -> lhs
      | Mult, LiteralExpr (Literal.IntLit i), _ when i = Bigint.one -> rhs
      | Div, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j)
        when j <> Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.(i / j))
      | Div, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.zero)
      | Div, _, LiteralExpr (Literal.IntLit i) when i = Bigint.one -> lhs
      | Div, lhs, rhs when [%compare.equal : u] lhs rhs ->
          mk_literal (Literal.IntLit Bigint.one)
      | Mod, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j)
        when j <> Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.(i - (i / j * j)))
      | Mod, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
          mk_literal (Literal.IntLit Bigint.zero)
      | And, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
          mk_literal (Literal.BoolLit (x && y))
      | And, LiteralExpr (Literal.BoolLit true), _ -> rhs
      | And, _, LiteralExpr (Literal.BoolLit true) -> lhs
      | And, LiteralExpr (Literal.BoolLit false), _ ->
          mk_literal (Literal.BoolLit false)
      | And, _, LiteralExpr (Literal.BoolLit false) ->
          mk_literal (Literal.BoolLit false)
      | Or, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
          mk_literal (Literal.BoolLit (x || y))
      | Or, LiteralExpr (Literal.BoolLit false), _ -> rhs
      | Or, _, LiteralExpr (Literal.BoolLit false) -> lhs
      | Or, LiteralExpr (Literal.BoolLit true), _ ->
          mk_literal (Literal.BoolLit true)
      | Or, _, LiteralExpr (Literal.BoolLit true) ->
          mk_literal (Literal.BoolLit true)
      | Imply, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
          mk_literal (Literal.BoolLit (not x || y))
      | Imply, LiteralExpr (Literal.BoolLit true), _ -> rhs
      | Imply, LiteralExpr (Literal.BoolLit false), _ ->
          mk_literal (Literal.BoolLit true)
      | Imply, _, LiteralExpr (Literal.BoolLit true) ->
          mk_literal (Literal.BoolLit true)
      | Lt, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.BoolLit Bigint.(i < j))
      | Eq, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.BoolLit Bigint.(i = j))
      | Eq, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
          mk_literal (Literal.BoolLit (x = y))
      | Ne, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
          mk_literal (Literal.BoolLit (not Bigint.(i = j)))
      | Ne, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
          mk_literal (Literal.BoolLit (not (x = y)))
      | _, _, _ -> mk_binary op (simplify lhs) (simplify rhs)

    let sexp_of_t e = Coeus.Expr.sexp_of_t (to_coeus e)

    let t_of_sexp _ =
      failwith
        "Cannot parse sexp to Ecoeus expression directly without going \
         through a type checker"

    let pp fmt e = Coeus.Expr.pp fmt (to_coeus e)
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module Lvalue = struct
  type t = {base: VarBinding.t; indices: Expr.t list; info: ExprInfo.t}
  [@@deriving sexp, compare, hash]

  let mk_var base =
    {base; indices= []; info= ExprInfo.{ast_size= 1; ty= base.ty}}

  let mk_array base indices ty =
    let ast_size = Expr.ast_size_of_exprs indices in
    {base; indices; info= ExprInfo.{ast_size; ty}}

  let type_of {info= ExprInfo.({ty; _}); _} = ty

  let ast_size_of {info= ExprInfo.({ast_size; _}); _} = ast_size

  let ast_size_of_lvalues lvals =
    List.fold lvals ~init:0 ~f:(fun acc lval -> acc + ast_size_of lval)

  let map_expr ~f lval =
    let indices' = List.map lval.indices ~f in
    mk_array lval.base indices' (type_of lval)

  let iter_expr ~f lval = List.iter lval.indices ~f

  let map_var ~f {base; indices; info} =
    let base = {base with name= f base.name} in
    let indices = List.map indices ~f:(Expr.map_var ~f) in
    {base; indices; info}

  let iter_var ~f {base; indices; _} =
    Expr.iter_var ~f (Expr.mk_var base) ;
    List.iter indices ~f:(Expr.iter_var ~f)

  let read_vars_impl read_vars {indices; _} =
    List.iter indices ~f:(Expr.iter_var ~f:(Hash_set.add read_vars))

  let write_vars_impl write_vars {base; _} = Hash_set.add write_vars base

  let read_write_vars_impl rw_vars lval =
    read_vars_impl rw_vars lval ;
    write_vars_impl rw_vars lval

  let to_coeus {base; indices; _} =
    let base' = base.name in
    let indices' = List.map indices ~f:Expr.to_coeus in
    Coeus.Lvalue.{base= base'; indices= indices'}

  let pp fmt {base; indices; _} =
    if List.is_empty indices then Identifier.pp fmt base.name
    else
      Fmt.pf fmt "%a[%a]" Identifier.pp base.name
        (Fmt.list Expr.pp ~sep:pp_comma)
        indices
end

module StmtInfo = struct
  type t = {ast_size: int; parent: Identifier.t}
  [@@deriving sexp, compare, hash]

  let pp fmt {ast_size; parent} =
    Fmt.pf fmt "StmtInfo{ ast_size = %d; parent = %a }" ast_size Identifier.pp
      parent
end

module Stmt = struct
  module ForDirection = Coeus.Stmt.ForDirection

  type t = {bare_stmt: u; info: StmtInfo.t} [@@deriving sexp, compare, hash]

  and u =
    | Assume of Expr.t
    | Assign of {lhs: Lvalue.t; rhs: Expr.t}
    | If of {cond: Expr.t; then_branch: t list; else_branch: t list}
    | While of {cond: Expr.t; body: t list}
    | For of
        { counter: Identifier.t
        ; lower: Expr.t
        ; upper: Expr.t
        ; step: Expr.t
        ; direction: ForDirection.t
        ; body: t list }
    | Call of {rets: Lvalue.t list; name: Identifier.t; args: Expr.t list}
  [@@deriving sexp, compare, hash]

  let ast_size_of {info= StmtInfo.({ast_size; _}); _} = ast_size

  let ast_size_of_stmts =
    List.fold ~init:0 ~f:(fun acc stmt -> acc + ast_size_of stmt)

  let parent_of {info= StmtInfo.({parent; _}); _} = parent

  let is_simple s =
    match s.bare_stmt with
    | Assume _ | Assign _ -> true
    | If _ | While _ | For _ | Call _ -> false

  let is_loop s =
    match s.bare_stmt with
    | While _ | For _ -> true
    | Assume _ | Assign _ | If _ | Call _ -> false

  let is_branch s =
    match s.bare_stmt with
    | If _ -> true
    | Assume _ | Assign _ | While _ | For _ | Call _ -> false

  let is_call s =
    match s.bare_stmt with
    | Call _ -> true
    | Assume _ | Assign _ | If _ | While _ | For _ -> false

  let rec count s =
    match s.bare_stmt with
    | Assume _ | Assign _ | Call _ -> 1
    | If {then_branch; else_branch; _} ->
        1 + count_stmts then_branch + count_stmts else_branch
    | While {body; _} -> 1 + count_stmts body
    | For {body; _} -> 1 + count_stmts body

  and count_stmts stmts =
    List.fold stmts ~init:0 ~f:(fun acc s -> acc + count s)

  let rec loop_nest_level_of s =
    match s.bare_stmt with
    | Assume _ | Assign _ | If _ | Call _ -> 0
    | While {body; _} | For {body; _} -> 1 + loop_nest_level_of_stmts body

  and loop_nest_level_of_stmts stmts =
    let levels = List.map stmts ~f:loop_nest_level_of in
    match List.max_elt levels ~compare:Int.compare with
    | Some l -> l
    | None -> 0

  let mk_assume parent e =
    let ast_size = 1 + Expr.ast_size_of e in
    {bare_stmt= Assume e; info= StmtInfo.{parent; ast_size}}

  let mk_assign parent lhs rhs =
    let ast_size = 1 + Lvalue.ast_size_of lhs + Expr.ast_size_of rhs in
    {bare_stmt= Assign {lhs; rhs}; info= StmtInfo.{parent; ast_size}}

  let mk_if parent cond then_branch else_branch =
    let ast_size =
      1 + Expr.ast_size_of cond
      + ast_size_of_stmts then_branch
      + ast_size_of_stmts else_branch
    in
    { bare_stmt= If {cond; then_branch; else_branch}
    ; info= StmtInfo.{parent; ast_size} }

  let mk_while parent cond body =
    let ast_size = 1 + Expr.ast_size_of cond + ast_size_of_stmts body in
    {bare_stmt= While {cond; body}; info= StmtInfo.{parent; ast_size}}

  let mk_for parent counter lower upper step direction body =
    let ast_size =
      1 + Expr.ast_size_of lower + Expr.ast_size_of upper
      + Expr.ast_size_of step + ast_size_of_stmts body
    in
    { bare_stmt= For {counter; lower; upper; step; direction; body}
    ; info= StmtInfo.{parent; ast_size} }

  let mk_call parent rets name args =
    let ast_size =
      1 + Lvalue.ast_size_of_lvalues rets + Expr.ast_size_of_exprs args
    in
    {bare_stmt= Call {rets; name; args}; info= StmtInfo.{parent; ast_size}}

  let with_parent stmt parent =
    let info = StmtInfo.{stmt.info with parent} in
    {stmt with info}

  let while_of_for parent counter lower upper step direction body =
    let counter_binding = VarBinding.{name= counter; ty= Type.IntType} in
    let counter_lval = Lvalue.mk_var counter_binding in
    let cond_op, update_op =
      match direction with
      | ForDirection.Forward -> (BinaryOperator.Lt, BinaryOperator.Plus)
      | ForDirection.Backward -> (BinaryOperator.Gt, BinaryOperator.Minus)
    in
    let cond = Expr.mk_binary cond_op (Expr.mk_var counter_binding) upper in
    let update_expr =
      Expr.mk_binary update_op (Expr.mk_var counter_binding) step
    in
    let update_stmt = mk_assign parent counter_lval update_expr in
    let body = List.append body [update_stmt] in
    (counter_lval, lower, cond, body)

  let rec map_expr ~f stmt =
    let parent = parent_of stmt in
    match stmt.bare_stmt with
    | Assume e ->
        let e' = f e in
        mk_assume parent e'
    | Assign {lhs; rhs} ->
        let lhs' = Lvalue.map_expr ~f lhs in
        let rhs' = f rhs in
        mk_assign parent lhs' rhs'
    | If {cond; then_branch; else_branch} ->
        let cond' = f cond in
        let then_branch' = List.map then_branch ~f:(map_expr ~f) in
        let else_branch' = List.map else_branch ~f:(map_expr ~f) in
        mk_if parent cond' then_branch' else_branch'
    | While {cond; body} ->
        let cond' = f cond in
        let body' = List.map body ~f:(map_expr ~f) in
        mk_while parent cond' body'
    | For {counter; lower; upper; step; direction; body} ->
        let lower' = f lower in
        let upper' = f upper in
        let step' = f step in
        let body' = List.map body ~f:(map_expr ~f) in
        mk_for parent counter lower' upper' step' direction body'
    | Call {rets; name; args} ->
        let rets' = List.map rets ~f:(Lvalue.map_expr ~f) in
        let args' = List.map args ~f in
        mk_call parent rets' name args'

  let rec iter_expr ~f stmt =
    match stmt.bare_stmt with
    | Assume e -> f e
    | Assign {lhs; rhs} -> Lvalue.iter_expr ~f lhs ; f rhs
    | If {cond; then_branch; else_branch} ->
        f cond ;
        List.iter then_branch ~f:(iter_expr ~f) ;
        List.iter else_branch ~f:(iter_expr ~f)
    | While {cond; body} ->
        f cond ;
        List.iter body ~f:(iter_expr ~f)
    | For {lower; upper; step; body; _} ->
        f lower ;
        f upper ;
        f step ;
        List.iter body ~f:(iter_expr ~f)
    | Call {rets; args; _} ->
        List.iter rets ~f:(Lvalue.iter_expr ~f) ;
        List.iter args ~f

  let rec map_var ~f stmt =
    match stmt.bare_stmt with
    | Assume e ->
        let bare_stmt = Assume (Expr.map_var ~f e) in
        {stmt with bare_stmt}
    | Assign {lhs; rhs} ->
        let bare_stmt =
          Assign {lhs= Lvalue.map_var ~f lhs; rhs= Expr.map_var ~f rhs}
        in
        {stmt with bare_stmt}
    | If {cond; then_branch; else_branch} ->
        let bare_stmt =
          If
            { cond= Expr.map_var ~f cond
            ; then_branch= List.map then_branch ~f:(map_var ~f)
            ; else_branch= List.map else_branch ~f:(map_var ~f) }
        in
        {stmt with bare_stmt}
    | While {cond; body} ->
        let bare_stmt =
          While
            {cond= Expr.map_var ~f cond; body= List.map body ~f:(map_var ~f)}
        in
        {stmt with bare_stmt}
    | For {counter; lower; upper; step; direction; body} ->
        let bare_stmt =
          For
            { counter= f counter
            ; lower= Expr.map_var ~f lower
            ; upper= Expr.map_var ~f upper
            ; step= Expr.map_var ~f step
            ; direction
            ; body= List.map body ~f:(map_var ~f) }
        in
        {stmt with bare_stmt}
    | Call {rets; name; args} ->
        let bare_stmt =
          Call
            { rets= List.map rets ~f:(Lvalue.map_var ~f)
            ; name
            ; args= List.map args ~f:(Expr.map_var ~f) }
        in
        {stmt with bare_stmt}

  let rec iter_var ~f stmt =
    match stmt.bare_stmt with
    | Assume e -> Expr.iter_var ~f e
    | Assign {lhs; rhs} -> Lvalue.iter_var ~f lhs ; Expr.iter_var ~f rhs
    | If {cond; then_branch; else_branch} ->
        Expr.iter_var ~f cond ;
        List.iter then_branch ~f:(iter_var ~f) ;
        List.iter else_branch ~f:(iter_var ~f)
    | While {cond; body} ->
        Expr.iter_var ~f cond ;
        List.iter body ~f:(iter_var ~f)
    | For {counter; lower; upper; step; body; _} ->
        Expr.iter_var ~f
          (Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}) ;
        Expr.iter_var ~f lower ;
        Expr.iter_var ~f upper ;
        Expr.iter_var ~f step ;
        List.iter body ~f:(iter_var ~f)
    | Call {rets; args; _} ->
        List.iter rets ~f:(Lvalue.iter_var ~f) ;
        List.iter args ~f:(Expr.iter_var ~f)

  let rec read_vars_impl ~exclude_counter read_vars stmt =
    let add_var = Hash_set.add read_vars in
    let add_vars_in_expr = Expr.iter_var ~f:add_var in
    match stmt.bare_stmt with
    | Assume e -> add_vars_in_expr e
    | Assign {lhs; rhs} ->
        Lvalue.read_vars_impl read_vars lhs ;
        add_vars_in_expr rhs
    | If {cond; then_branch; else_branch} ->
        add_vars_in_expr cond ;
        read_vars_stmts_impl ~exclude_counter read_vars then_branch ;
        read_vars_stmts_impl ~exclude_counter read_vars else_branch
    | While {cond; body} ->
        add_vars_in_expr cond ;
        read_vars_stmts_impl ~exclude_counter read_vars body
    | For {counter; lower; upper; step; body; _} ->
        add_vars_in_expr lower ;
        add_vars_in_expr upper ;
        add_vars_in_expr step ;
        if not exclude_counter then
          Hash_set.add read_vars VarBinding.{name= counter; ty= Type.IntType} ;
        read_vars_stmts_impl ~exclude_counter read_vars body
    | Call {rets; args; _} ->
        List.iter rets ~f:(Lvalue.read_vars_impl read_vars) ;
        List.iter args ~f:add_vars_in_expr

  and read_vars_stmts_impl ~exclude_counter read_vars =
    List.iter ~f:(read_vars_impl ~exclude_counter read_vars)

  let read_var_set_of ?(exclude_counter= false) stmt =
    let read_vars = VarBinding.Hash_set.create ~size:16 () in
    read_vars_impl ~exclude_counter read_vars stmt ;
    read_vars

  let rec write_vars_impl ~exclude_counter write_vars stmt =
    let add_var = Hash_set.add write_vars in
    match stmt.bare_stmt with
    | Assume _ -> ()
    | Assign {lhs; _} -> Lvalue.write_vars_impl write_vars lhs
    | If {then_branch; else_branch; _} ->
        write_vars_stmts_impl ~exclude_counter write_vars then_branch ;
        write_vars_stmts_impl ~exclude_counter write_vars else_branch
    | While {body; _} -> write_vars_stmts_impl ~exclude_counter write_vars body
    | For {counter; body; _} ->
        if not exclude_counter then
          add_var VarBinding.{name= counter; ty= Type.IntType} ;
        write_vars_stmts_impl ~exclude_counter write_vars body
    | Call {rets; _} -> List.iter rets ~f:(Lvalue.write_vars_impl write_vars)

  and write_vars_stmts_impl ~exclude_counter write_vars =
    List.iter ~f:(write_vars_impl ~exclude_counter write_vars)

  let write_var_set_of ?(exclude_counter= false) stmt =
    let write_vars = VarBinding.Hash_set.create ~size:16 () in
    write_vars_impl ~exclude_counter write_vars stmt ;
    write_vars

  let rec read_write_vars_impl ~exclude_counter rw_vars stmt =
    let add_var = Hash_set.add rw_vars in
    let add_vars_in_expr = Expr.iter_var ~f:add_var in
    match stmt.bare_stmt with
    | Assume e -> add_vars_in_expr e
    | Assign {lhs; rhs} ->
        Lvalue.read_write_vars_impl rw_vars lhs ;
        add_vars_in_expr rhs
    | If {cond; then_branch; else_branch} ->
        add_vars_in_expr cond ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars then_branch ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars else_branch
    | While {cond; body} ->
        add_vars_in_expr cond ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars body
    | For {counter; lower; upper; step; body; _} ->
        add_vars_in_expr lower ;
        add_vars_in_expr upper ;
        add_vars_in_expr step ;
        if not exclude_counter then
          add_var VarBinding.{name= counter; ty= Type.IntType} ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars body
    | Call {rets; args; _} ->
        List.iter rets ~f:(Lvalue.read_write_vars_impl rw_vars) ;
        List.iter args ~f:add_vars_in_expr

  and read_write_vars_stmts_impl ~exclude_counter rw_vars =
    List.iter ~f:(read_write_vars_impl ~exclude_counter rw_vars)

  let read_write_var_set_of ?(exclude_counter= false) stmt =
    let rw_vars = VarBinding.Hash_set.create ~size:32 () in
    read_write_vars_impl ~exclude_counter rw_vars stmt ;
    rw_vars

  let rec conds_of_impl expr_set stmt =
    match stmt.bare_stmt with
    | Assume _ | Assign _ | Call _ -> ()
    | If {cond; then_branch; else_branch} ->
        Hash_set.add expr_set cond ;
        conds_of_stmts_impl expr_set then_branch ;
        conds_of_stmts_impl expr_set else_branch
    | While {cond; body} ->
        Hash_set.add expr_set cond ;
        conds_of_stmts_impl expr_set body
    | For {counter; lower; upper; direction; body; _} ->
        let counter_expr =
          Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
        in
        let cond =
          match direction with
          | ForDirection.Forward ->
              Expr.mk_binary BinaryOperator.Lt counter_expr upper
          | ForDirection.Backward ->
              Expr.mk_binary BinaryOperator.Gt counter_expr lower
        in
        Hash_set.add expr_set cond ;
        conds_of_stmts_impl expr_set body

  and conds_of_stmts_impl expr_set stmts =
    List.iter stmts ~f:(conds_of_impl expr_set)

  let conds_of stmt =
    let cond_set = Expr.Hash_set.create ~size:8 () in
    conds_of_impl cond_set stmt ;
    Hash_set.to_list cond_set

  let rec assign_rhs_of_impl expr_set stmt =
    match stmt.bare_stmt with
    | Assume _ | Call _ -> ()
    | Assign {rhs; _} -> Hash_set.add expr_set rhs
    | If {then_branch; else_branch; _} ->
        assign_rhs_of_stmts_impl expr_set then_branch ;
        assign_rhs_of_stmts_impl expr_set else_branch
    | While {body; _} -> assign_rhs_of_stmts_impl expr_set body
    | For {counter; lower; step; direction; body; _} ->
        let counter_expr =
          Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
        in
        let cond =
          match direction with
          | ForDirection.Forward ->
              Expr.mk_binary BinaryOperator.Plus counter_expr step
          | ForDirection.Backward ->
              Expr.mk_binary BinaryOperator.Minus counter_expr step
        in
        Hash_set.add expr_set lower ;
        Hash_set.add expr_set cond ;
        assign_rhs_of_stmts_impl expr_set body

  and assign_rhs_of_stmts_impl expr_set stmts =
    List.iter stmts ~f:(assign_rhs_of_impl expr_set)

  let assign_rhs_of stmt =
    let expr_set = Expr.Hash_set.create ~size:8 () in
    assign_rhs_of_impl expr_set stmt ;
    Hash_set.to_list expr_set

  let rec to_coeus stmt =
    match stmt.bare_stmt with
    | Assume e -> Coeus.Stmt.Assume (Expr.to_coeus e)
    | Assign {lhs; rhs} ->
        Coeus.Stmt.Assign {lhs= Lvalue.to_coeus lhs; rhs= Expr.to_coeus rhs}
    | If {cond; then_branch; else_branch} ->
        Coeus.Stmt.If
          { cond= Expr.to_coeus cond
          ; then_branch= List.map then_branch ~f:to_coeus
          ; else_branch= List.map else_branch ~f:to_coeus }
    | While {cond; body} ->
        Coeus.Stmt.While
          {cond= Expr.to_coeus cond; body= List.map body ~f:to_coeus}
    | For {counter; lower; upper; step; direction; body} ->
        Coeus.Stmt.For
          { counter
          ; lower= Expr.to_coeus lower
          ; upper= Expr.to_coeus upper
          ; step= Expr.to_coeus step
          ; direction
          ; body= List.map body ~f:to_coeus }
    | Call {rets; name; args} ->
        Coeus.Stmt.Call
          { rets= List.map rets ~f:Lvalue.to_coeus
          ; name
          ; args= List.map args ~f:Expr.to_coeus }

  let pp fmt stmt = Coeus.Stmt.pp fmt (to_coeus stmt)

  let pp_brief fmt stmt = Coeus.Stmt.pp_brief fmt (to_coeus stmt)
end

module Stmts = struct
  type t = Stmt.t list [@@deriving sexp, compare, hash]

  let ast_size_of = Stmt.ast_size_of_stmts

  let count = Stmt.count_stmts

  let loop_nest_level_of = Stmt.loop_nest_level_of_stmts

  let with_parent ss p = List.map ss ~f:(fun s -> Stmt.with_parent s p)

  let iter_var ~f = List.iter ~f:(Stmt.iter_var ~f)

  let map_var ~f = List.map ~f:(Stmt.map_var ~f)

  let iter_expr ~f = List.iter ~f:(Stmt.iter_expr ~f)

  let map_expr ~f = List.map ~f:(Stmt.map_expr ~f)

  let read_var_set_of ?(exclude_counter= false) stmts =
    let read_vars = VarBinding.Hash_set.create ~size:64 () in
    Stmt.read_vars_stmts_impl ~exclude_counter read_vars stmts ;
    read_vars

  let write_var_set_of ?(exclude_counter= false) stmts =
    let write_vars = VarBinding.Hash_set.create ~size:64 () in
    Stmt.write_vars_stmts_impl ~exclude_counter write_vars stmts ;
    write_vars

  let read_write_var_set_of ?(exclude_counter= false) stmts =
    let rw_vars = VarBinding.Hash_set.create ~size:128 () in
    Stmt.read_write_vars_stmts_impl ~exclude_counter rw_vars stmts ;
    rw_vars

  let conds_of stmts =
    let cond_set = Expr.Hash_set.create ~size:32 () in
    Stmt.conds_of_stmts_impl cond_set stmts ;
    Hash_set.to_list cond_set

  let assign_rhs_of stmts =
    let expr_set = Expr.Hash_set.create ~size:32 () in
    Stmt.assign_rhs_of_stmts_impl expr_set stmts ;
    Hash_set.to_list expr_set

  let to_coeus = List.map ~f:Stmt.to_coeus

  let pp fmt stmts = Coeus.Stmts.pp fmt (List.map stmts ~f:Stmt.to_coeus)

  let pp_brief fmt stmts =
    Coeus.Stmts.pp_brief fmt (List.map stmts ~f:Stmt.to_coeus)
end

module Procedure = struct
  type t =
    { name: Identifier.t
    ; params: VarBinding.t list
    ; rets: VarBinding.t list
    ; locals: VarBinding.t list
    ; stmts: Stmt.t list }
  [@@deriving sexp, compare, hash]

  let to_coeus {name; params; rets; locals; stmts} =
    let stmts = Stmts.to_coeus stmts in
    Coeus.Procedure.{name; params; rets; locals; stmts}
end

module Spec = struct
  type t = {requires: Expr.t list; ensures: Expr.t list}
  [@@deriving sexp, compare, hash]

  let to_coeus {requires; ensures} =
    let requires' = List.map requires ~f:Expr.to_coeus in
    let ensures' = List.map ensures ~f:Expr.to_coeus in
    Coeus.Spec.{requires= requires'; ensures= ensures'}
end

type t = {procs: Procedure.t list; entry: EntrySpec.t; spec: Spec.t}
[@@deriving sexp, compare, hash]

let collect_fundecls procs =
  let fun_set = FunDecl.Hash_set.create ~size:16 () in
  let collect_stmts_fundecls stmts =
    let rec find_decl e =
      let open Expr in
      match e.bare_expr with
      | LiteralExpr _ | VarExpr _ -> ()
      | UnaryExpr (_, e) -> find_decl e
      | BinaryExpr (_, lhs, rhs) -> find_decl lhs ; find_decl rhs
      | ArraySelectExpr {base; indices} ->
          find_decl base ;
          List.iter indices ~f:find_decl
      | ArrayStoreExpr {base; indices; value} ->
          find_decl base ;
          List.iter indices ~f:find_decl ;
          find_decl value
      | CondExpr {cond; true_val; false_val} ->
          find_decl cond ; find_decl true_val ; find_decl false_val
      | FunCallExpr {func; args} ->
          Core.Hash_set.add fun_set func ;
          List.iter args ~f:find_decl
      | QuantifiedExpr {body; _} -> find_decl body
    in
    Stmts.iter_expr stmts ~f:find_decl
  in
  List.iter procs ~f:(fun Procedure.({stmts; _}) ->
      collect_stmts_fundecls stmts ) ;
  Hash_set.to_list fun_set

let to_coeus {procs; entry; spec} =
  let decls = collect_fundecls procs in
  let procs = List.map procs ~f:Procedure.to_coeus in
  let spec = Spec.to_coeus spec in
  Coeus.{procs; decls; entry; spec}

let lookup_proc {procs; _} id =
  List.find procs ~f:(fun proc -> Identifier.equal id proc.Procedure.name)

let lookup_proc_exn prog id =
  match lookup_proc prog id with
  | Some proc -> proc
  | None ->
      let msg =
        Fmt.strf "ECoeus procedure lookup failed: %a" Identifier.pp id
      in
      raise (Not_found_s (Sexp.Atom msg))

let update_proc prog proc =
  let id = proc.Procedure.name in
  let procs' =
    List.filter prog.procs ~f:(fun proc ->
        not (Identifier.equal id proc.Procedure.name) )
  in
  let procs = proc :: procs' in
  {prog with procs}
