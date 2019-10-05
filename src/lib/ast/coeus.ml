open Core

module Identifier = struct
  module T = struct
    type t = string [@@deriving sexp, compare, hash]

    let of_string i = i

    let string_of i = i

    let pp = Fmt.string
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module Quantifier = struct
  type t = Exists | ForAll [@@deriving compare, hash]

  let sexp_of_t = function
    | Exists -> Sexp.Atom "exists"
    | ForAll -> Sexp.Atom "forall"

  let t_of_sexp = function
    | Sexp.Atom "exists" -> Exists
    | Sexp.Atom "forall" -> ForAll
    | _ as sexp ->
        let msg = "Unrecognized quantifier sexp" in
        raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

  let pp fmt = function
    | Exists -> Fmt.string fmt "exists"
    | ForAll -> Fmt.string fmt "forall"
end

module Side = struct
  type t = Left | Right [@@deriving sexp, compare, hash]

  let equal x y = compare x y = 0

  let pp fmt = function
    | Left -> Fmt.string fmt "$L"
    | Right -> Fmt.string fmt "$R"
end

module Type = struct
  module T = struct
    type t = BoolType | IntType | ArrayType of t list * t
    [@@deriving compare, hash]

    let is_array_type = function ArrayType _ -> true | _ -> false

    let rec sexp_of_t = function
      | BoolType -> Sexp.Atom "Bool"
      | IntType -> Sexp.Atom "Int"
      | ArrayType (key_tys, val_ty) ->
          (* Z3 Spacer has some problems with flat-encoded arrays. Use nested encoding *)
          let val_sexp = sexp_of_t val_ty in
          List.fold key_tys ~init:val_sexp ~f:(fun acc key_ty ->
              let key_sexp = sexp_of_t key_ty in
              Sexp.List [Sexp.Atom "Array"; key_sexp; acc] )

    (* let all_tys = List.append key_tys [val_ty] in
           * let first = Sexp.Atom "Array" in
           * Sexp.List (first :: List.map all_tys ~f:sexp_of_t) *)

    let rec t_of_sexp sexp =
      match sexp with
      | Sexp.Atom satom when String.equal satom "Bool" -> BoolType
      | Sexp.Atom satom when String.equal satom "Int" -> IntType
      | Sexp.List (Sexp.Atom "Array" :: rest) -> (
          let rest_tys = List.map rest ~f:t_of_sexp in
          match List.split_n rest_tys (List.length rest_tys - 1) with
          | key_tys, [val_ty] -> ArrayType (key_tys, val_ty)
          | _ ->
              let msg = "Unrecognized array type sexp" in
              raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp)) )
      | _ ->
          let msg = "Unrecognized type sexp" in
          raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

    let rec pp fmt = function
      | BoolType -> Fmt.string fmt "bool"
      | IntType -> Fmt.string fmt "int"
      | ArrayType (key_tys, val_ty) ->
          Fmt.pf fmt "%a[%a]" pp val_ty (Fmt.list ~sep:Fmt.comma pp) key_tys
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module UnaryOperator = struct
  type t = Neg | Not [@@deriving sexp, compare, hash]

  let ret_type_of = function Neg -> Type.IntType | Not -> Type.BoolType

  let op_type_of = function Neg -> Type.IntType | Not -> Type.BoolType

  let pp fmt = function Neg -> Fmt.string fmt "-" | Not -> Fmt.string fmt "!"
end

module BinaryOperator = struct
  type t =
    (* Arithmetic *)
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    (* Logical *)
    | And
    | Or
    | Imply
    (* Relational *)
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Ne
  [@@deriving sexp, compare, hash]

  let ret_type_of = function
    | Plus | Minus | Mult | Div | Mod -> Type.IntType
    | And | Or | Imply | Lt | Le | Gt | Ge | Eq | Ne -> Type.BoolType

  let op_type_of = function
    | Plus | Minus | Mult | Div | Mod | Lt | Le | Gt | Ge | Eq | Ne ->
        (Type.IntType, Type.IntType)
    | And | Or | Imply -> (Type.BoolType, Type.BoolType)

  let pp fmt = function
    | Plus -> Fmt.string fmt "+"
    | Minus -> Fmt.string fmt "-"
    | Mult -> Fmt.string fmt "*"
    | Div -> Fmt.string fmt "/"
    | Mod -> Fmt.string fmt "%"
    | And -> Fmt.string fmt "&&"
    | Or -> Fmt.string fmt "||"
    | Imply -> Fmt.string fmt "==>"
    | Lt -> Fmt.string fmt "<"
    | Le -> Fmt.string fmt "<="
    | Gt -> Fmt.string fmt ">"
    | Ge -> Fmt.string fmt ">="
    | Eq -> Fmt.string fmt "=="
    | Ne -> Fmt.string fmt "!="
end

module Literal = struct
  module T = struct
    type t = BoolLit of bool | IntLit of Bigint.t
    [@@deriving sexp, compare, hash]

    let pp fmt = function
      | BoolLit b -> Fmt.bool fmt b
      | IntLit i -> Fmt.pf fmt "%s" (Bigint.to_string i)

    let sexp_of_t = function
      | BoolLit false -> Sexp.Atom "false"
      | BoolLit true -> Sexp.Atom "true"
      | IntLit i ->
          if Bigint.is_negative i then
            let i' = Bigint.neg i in
            let s = Bigint.to_string i' in
            Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_string ["-"; "0"; s]
          else
            let s = Bigint.to_string i in
            Sexplib.Conv.sexp_of_string s

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom "false" -> BoolLit false
      | Sexp.Atom "true" -> BoolLit true
      | Sexp.Atom digits when String.for_all digits ~f:Char.is_digit ->
          IntLit (Bigint.of_string digits)
      | _ ->
          let msg = "Unrecognized literal sexp" in
          raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

    let type_of = function
      | BoolLit _ -> Type.BoolType
      | IntLit _ -> Type.IntType

    let of_int i = IntLit (Bigint.of_int i)

    let of_bool b = BoolLit b
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module VarBinding = struct
  module T = struct
    type t = {name: Identifier.t; ty: Type.t} [@@deriving compare, hash]

    let sexp_of_t {name; ty} =
      Sexp.List [Identifier.sexp_of_t name; Type.sexp_of_t ty]

    let t_of_sexp sexp =
      match sexp with
      | Sexp.List [name_sexp; ty_sexp] ->
          {name= Identifier.t_of_sexp name_sexp; ty= Type.t_of_sexp ty_sexp}
      | _ ->
          let msg = "Unrecognized var binding sexp" in
          raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

    let pp fmt {name; ty} = Fmt.pf fmt "%a %a" Type.pp ty Identifier.pp name
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module Expr = struct
  type t =
    | LiteralExpr of Literal.t
    | VarExpr of Identifier.t
    | UnaryExpr of UnaryOperator.t * t
    | BinaryExpr of BinaryOperator.t * t * t
    | ArraySelectExpr of {base: t; indices: t list}
    | ArrayStoreExpr of {base: t; indices: t list; value: t}
    | AnnotatedExpr of {annot: Side.t; expr: t}
    | CondExpr of {cond: t; true_val: t; false_val: t}
    | FunCallExpr of {name: Identifier.t; args: t list}
    | QuantifiedExpr of
        { quantifier: Quantifier.t
        ; bindings: VarBinding.t list
        ; body: t }
  [@@deriving compare, hash]

  let equal = [%compare.equal: t]

  let logical_negate_of = function
    | LiteralExpr (Literal.BoolLit true) -> LiteralExpr (Literal.BoolLit false)
    | LiteralExpr (Literal.BoolLit false) -> LiteralExpr (Literal.BoolLit true)
    | UnaryExpr (UnaryOperator.Not, e) -> e
    | BinaryExpr (BinaryOperator.Le, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Gt, lhs, rhs)
    | BinaryExpr (BinaryOperator.Lt, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Ge, lhs, rhs)
    | BinaryExpr (BinaryOperator.Ge, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Lt, lhs, rhs)
    | BinaryExpr (BinaryOperator.Gt, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Le, lhs, rhs)
    | BinaryExpr (BinaryOperator.Eq, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Ne, lhs, rhs)
    | BinaryExpr (BinaryOperator.Ne, lhs, rhs) ->
        BinaryExpr (BinaryOperator.Eq, lhs, rhs)
    | _ as e -> UnaryExpr (UnaryOperator.Not, e)

  let rec simplify = function
    | (LiteralExpr _ | VarExpr _) as e -> e
    | UnaryExpr (op, e) -> simplify_unary op e
    | BinaryExpr (op, lhs, rhs) -> simplify_binary op lhs rhs
    | ArraySelectExpr {base; indices} -> (
        let base = simplify base in
        let indices = List.map indices ~f:simplify in
        match base with
        | ArrayStoreExpr {indices= store_indices; value; _}
          when List.equal indices store_indices ~equal ->
            value
        | _ -> ArraySelectExpr {base; indices} )
    | ArrayStoreExpr {base; indices; value} -> (
        let base = simplify base in
        let indices = List.map indices ~f:simplify in
        let value = simplify value in
        match value with
        | ArraySelectExpr {base= select_base; indices= select_indices}
          when equal base select_base
               && List.equal indices select_indices ~equal ->
            base
        | _ -> ArrayStoreExpr {base; indices; value} )
    | AnnotatedExpr {annot; expr} -> AnnotatedExpr {annot; expr= simplify expr}
    | CondExpr {cond; true_val; false_val} -> (
      match simplify cond with
      | LiteralExpr (Literal.BoolLit true) -> simplify true_val
      | LiteralExpr (Literal.BoolLit false) -> simplify false_val
      | _ as cond ->
          CondExpr
            {cond; true_val= simplify true_val; false_val= simplify false_val}
      )
    | FunCallExpr {name; args} ->
        FunCallExpr {name; args= List.map args ~f:simplify}
    | QuantifiedExpr {quantifier; bindings; body} ->
        QuantifiedExpr {quantifier; bindings; body= simplify body}

  and simplify_unary op e =
    let open UnaryOperator in
    match (op, e) with
    | Not, e -> logical_negate_of (simplify e)
    | Neg, LiteralExpr (Literal.IntLit i) ->
        LiteralExpr (Literal.IntLit (Bigint.neg i))
    | Neg, UnaryExpr (Neg, e) -> simplify e
    | _, _ -> UnaryExpr (op, simplify e)

  and simplify_binary op lhs rhs =
    let open BinaryOperator in
    match (op, lhs, rhs) with
    | Plus, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.IntLit Bigint.(i + j))
    | Plus, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero -> lhs
    | ( Plus
      , BinaryExpr (Plus, e, LiteralExpr (Literal.IntLit i))
      , LiteralExpr (Literal.IntLit j) ) ->
        simplify_binary Plus e (LiteralExpr (Literal.IntLit Bigint.(i + j)))
    | ( Plus
      , BinaryExpr (Minus, e, LiteralExpr (Literal.IntLit i))
      , LiteralExpr (Literal.IntLit j) ) ->
        simplify_binary Minus e (LiteralExpr (Literal.IntLit Bigint.(i - j)))
    | Plus, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero -> rhs
    | Plus, _, UnaryExpr (UnaryOperator.Neg, e) ->
        simplify (BinaryExpr (Minus, lhs, e))
    | Plus, BinaryExpr (Minus, e0, e1), e2 when equal e1 e2 -> simplify e0
    | Minus, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.IntLit Bigint.(i - j))
    | Minus, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero -> lhs
    | ( Minus
      , BinaryExpr (Plus, e, LiteralExpr (Literal.IntLit i))
      , LiteralExpr (Literal.IntLit j) ) ->
        simplify_binary Plus e (LiteralExpr (Literal.IntLit Bigint.(i - j)))
    | ( Minus
      , BinaryExpr (Minus, e, LiteralExpr (Literal.IntLit i))
      , LiteralExpr (Literal.IntLit j) ) ->
        simplify_binary Minus e (LiteralExpr (Literal.IntLit Bigint.(i + j)))
    | Minus, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
        simplify (UnaryExpr (UnaryOperator.Neg, rhs))
    | Minus, lhs, rhs when equal lhs rhs ->
        LiteralExpr (Literal.IntLit Bigint.zero)
    | Minus, _, UnaryExpr (UnaryOperator.Neg, e) ->
        simplify (BinaryExpr (Plus, lhs, e))
    | Minus, BinaryExpr (Plus, e0, e1), e2 when equal e0 e2 -> simplify e1
    | Minus, BinaryExpr (Plus, e0, e1), e2 when equal e1 e2 -> simplify e0
    | Mult, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.IntLit Bigint.(i * j))
    | Mult, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.zero)
    | Mult, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.zero)
    | Mult, _, LiteralExpr (Literal.IntLit i) when i = Bigint.one -> lhs
    | Mult, LiteralExpr (Literal.IntLit i), _ when i = Bigint.one -> rhs
    | Mult, LiteralExpr (Literal.IntLit i), e
      when Bigint.(i > zero && i < of_int 4) ->
        let e = simplify e in
        let rec plus acc = function
          | 0 -> acc
          | n ->
              let acc = BinaryExpr (BinaryOperator.Plus, e, acc) in
              let n = n - 1 in
              plus acc n
        in
        let j = Bigint.to_int_exn i in
        simplify (plus e (j - 1))
    | Mult, e, LiteralExpr (Literal.IntLit i)
      when Bigint.(i > zero && i < of_int 4) ->
        let e = simplify e in
        let rec plus acc = function
          | 0 -> acc
          | n ->
              let acc = BinaryExpr (BinaryOperator.Plus, e, acc) in
              let n = n - 1 in
              plus acc n
        in
        let j = Bigint.to_int_exn i in
        simplify (plus e (j - 1))
    | Div, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j)
      when j <> Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.(i / j))
    | Div, _, LiteralExpr (Literal.IntLit i) when i = Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.zero)
    | Div, _, LiteralExpr (Literal.IntLit i) when i = Bigint.one -> lhs
    | Div, lhs, rhs when equal lhs rhs ->
        LiteralExpr (Literal.IntLit Bigint.one)
    | Mod, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j)
      when j <> Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.(i - (i / j * j)))
    | Mod, LiteralExpr (Literal.IntLit i), _ when i = Bigint.zero ->
        LiteralExpr (Literal.IntLit Bigint.zero)
    | And, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
        LiteralExpr (Literal.BoolLit (x && y))
    | And, LiteralExpr (Literal.BoolLit true), _ -> rhs
    | And, _, LiteralExpr (Literal.BoolLit true) -> lhs
    | And, LiteralExpr (Literal.BoolLit false), _ ->
        LiteralExpr (Literal.BoolLit false)
    | And, _, LiteralExpr (Literal.BoolLit false) ->
        LiteralExpr (Literal.BoolLit false)
    | Or, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
        LiteralExpr (Literal.BoolLit (x || y))
    | Or, LiteralExpr (Literal.BoolLit false), _ -> rhs
    | Or, _, LiteralExpr (Literal.BoolLit false) -> lhs
    | Or, LiteralExpr (Literal.BoolLit true), _ ->
        LiteralExpr (Literal.BoolLit true)
    | Or, _, LiteralExpr (Literal.BoolLit true) ->
        LiteralExpr (Literal.BoolLit true)
    | Imply, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y)
      ->
        LiteralExpr (Literal.BoolLit ((not x) || y))
    | Imply, LiteralExpr (Literal.BoolLit true), _ -> rhs
    | Imply, LiteralExpr (Literal.BoolLit false), _ ->
        LiteralExpr (Literal.BoolLit true)
    | Imply, _, LiteralExpr (Literal.BoolLit true) ->
        LiteralExpr (Literal.BoolLit true)
    | Lt, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.BoolLit Bigint.(i < j))
    | Eq, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.BoolLit Bigint.(i = j))
    | Eq, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
        LiteralExpr (Literal.BoolLit (x = y))
    | Ne, LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j) ->
        LiteralExpr (Literal.BoolLit (not Bigint.(i = j)))
    | Ne, LiteralExpr (Literal.BoolLit x), LiteralExpr (Literal.BoolLit y) ->
        LiteralExpr (Literal.BoolLit (not (x = y)))
    | _, _, _ -> BinaryExpr (op, simplify lhs, simplify rhs)

  let subst subst_map expr =
    let process_var subst_map var =
      match Identifier.Map.find subst_map var with
      | None -> VarExpr var
      | Some e -> e
    in
    let rec run_expr subst_map expr =
      match expr with
      | LiteralExpr _ -> expr
      | VarExpr v -> process_var subst_map v
      | UnaryExpr (op, e) -> UnaryExpr (op, run_expr subst_map e)
      | BinaryExpr (op, lhs, rhs) ->
          BinaryExpr (op, run_expr subst_map lhs, run_expr subst_map rhs)
      | ArraySelectExpr {base; indices} ->
          let base' = run_expr subst_map base in
          let indices' = List.map indices ~f:(run_expr subst_map) in
          ArraySelectExpr {base= base'; indices= indices'}
      | ArrayStoreExpr {base; indices; value} ->
          let base' = run_expr subst_map base in
          let indices' = List.map indices ~f:(run_expr subst_map) in
          let value' = run_expr subst_map value in
          ArrayStoreExpr {base= base'; indices= indices'; value= value'}
      | AnnotatedExpr {annot; expr} ->
          AnnotatedExpr {annot; expr= run_expr subst_map expr}
      | CondExpr {cond; true_val; false_val} ->
          CondExpr
            { cond= run_expr subst_map cond
            ; true_val= run_expr subst_map true_val
            ; false_val= run_expr subst_map false_val }
      | FunCallExpr {name; args} ->
          let args = List.map args ~f:(run_expr subst_map) in
          FunCallExpr {name; args}
      | QuantifiedExpr {quantifier; bindings; body} ->
          let subst_map' =
            List.fold bindings ~init:subst_map
              ~f:(fun acc VarBinding.({name; _}) ->
                Identifier.Map.remove acc name )
          in
          let body' = run_expr subst_map' body in
          QuantifiedExpr {quantifier; bindings; body= body'}
    in
    run_expr subst_map expr

  let map_var ~f e =
    let rec run_var env = function
      | LiteralExpr _ as e -> e
      | VarExpr id as e ->
          if Identifier.Set.mem env id then e else VarExpr (f id)
      | UnaryExpr (op, e) -> UnaryExpr (op, run_var env e)
      | BinaryExpr (op, lhs, rhs) ->
          BinaryExpr (op, run_var env lhs, run_var env rhs)
      | ArraySelectExpr {base; indices} ->
          ArraySelectExpr
            {base= run_var env base; indices= List.map indices ~f:(run_var env)}
      | ArrayStoreExpr {base; indices; value} ->
          ArrayStoreExpr
            { base= run_var env base
            ; indices= List.map indices ~f:(run_var env)
            ; value= run_var env value }
      | AnnotatedExpr {annot; expr} ->
          AnnotatedExpr {annot; expr= run_var env expr}
      | CondExpr {cond; true_val; false_val} ->
          CondExpr
            { cond= run_var env cond
            ; true_val= run_var env true_val
            ; false_val= run_var env false_val }
      | FunCallExpr {name; args} ->
          FunCallExpr {name; args= List.map args ~f:(run_var env)}
      | QuantifiedExpr {quantifier; bindings; body} ->
          let env =
            List.fold bindings ~init:env ~f:(fun acc VarBinding.({name; _}) ->
                Identifier.Set.add acc name )
          in
          QuantifiedExpr {quantifier; bindings; body= run_var env body}
    in
    run_var Identifier.Set.empty e

  let rec free_vars_impl free_vars bound_vars = function
    | LiteralExpr _ -> ()
    | VarExpr v ->
        if not (Identifier.Set.mem bound_vars v) then Hash_set.add free_vars v
    | UnaryExpr (_, e) -> free_vars_impl free_vars bound_vars e
    | BinaryExpr (_, lhs, rhs) ->
        free_vars_impl free_vars bound_vars lhs ;
        free_vars_impl free_vars bound_vars rhs
    | ArraySelectExpr {base; indices} ->
        free_vars_impl free_vars bound_vars base ;
        List.iter indices ~f:(free_vars_impl free_vars bound_vars)
    | ArrayStoreExpr {base; indices; value} ->
        free_vars_impl free_vars bound_vars base ;
        List.iter indices ~f:(free_vars_impl free_vars bound_vars) ;
        free_vars_impl free_vars bound_vars value
    | AnnotatedExpr {expr; _} -> free_vars_impl free_vars bound_vars expr
    | CondExpr {cond; true_val; false_val} ->
        free_vars_impl free_vars bound_vars cond ;
        free_vars_impl free_vars bound_vars true_val ;
        free_vars_impl free_vars bound_vars false_val
    | FunCallExpr {args; _} ->
        List.iter args ~f:(free_vars_impl free_vars bound_vars)
    | QuantifiedExpr {bindings; body; _} ->
        let bound_vars =
          List.fold bindings ~init:bound_vars
            ~f:(fun acc VarBinding.({name; _}) -> Identifier.Set.add acc name
          )
        in
        free_vars_impl free_vars bound_vars body

  let free_var_set_of expr =
    let free_vars = Identifier.Hash_set.create ~size:16 () in
    let bound_vars = Identifier.Set.empty in
    free_vars_impl free_vars bound_vars expr ;
    free_vars

  let free_var_set_of_exprs exprs =
    let free_vars = Identifier.Hash_set.create ~size:64 () in
    let bound_vars = Identifier.Set.empty in
    List.iter exprs ~f:(fun e -> free_vars_impl free_vars bound_vars e) ;
    free_vars

  let free_vars_of expr =
    let free_var_set = free_var_set_of expr in
    Hash_set.to_list free_var_set

  let rec sexp_of_t = function
    | LiteralExpr lit -> Literal.sexp_of_t lit
    | VarExpr id -> Identifier.sexp_of_t id
    | UnaryExpr (op, e) ->
        let op_sexp =
          match op with
          | UnaryOperator.Neg -> Sexplib.Conv.sexp_of_string "-"
          | UnaryOperator.Not -> Sexplib.Conv.sexp_of_string "not"
        in
        Sexp.List [op_sexp; sexp_of_t e]
    | BinaryExpr (op, lhs, rhs) -> (
        let build_binop_sexp name lhs rhs =
          let op_sexp = Sexp.Atom name in
          Sexp.List [op_sexp; sexp_of_t lhs; sexp_of_t rhs]
        in
        let open BinaryOperator in
        match op with
        | Plus -> build_binop_sexp "+" lhs rhs
        | Minus -> build_binop_sexp "-" lhs rhs
        | Mult -> build_binop_sexp "*" lhs rhs
        | Div -> build_binop_sexp "div" lhs rhs
        | Mod -> build_binop_sexp "mod" lhs rhs
        | And -> build_binop_sexp "and" lhs rhs
        | Or -> build_binop_sexp "or" lhs rhs
        | Imply -> build_binop_sexp "=>" lhs rhs
        | Lt -> build_binop_sexp "<" lhs rhs
        | Le -> build_binop_sexp "<=" lhs rhs
        | Gt -> build_binop_sexp ">" lhs rhs
        | Ge -> build_binop_sexp ">=" lhs rhs
        | Eq -> build_binop_sexp "=" lhs rhs
        | Ne ->
            let eq_sexp = build_binop_sexp "=" lhs rhs in
            Sexp.List [Sexplib.Conv.sexp_of_string "not"; eq_sexp] )
    | ArraySelectExpr {base; indices} ->
        let base_sexp = sexp_of_t base in
        let index_sexps = List.map indices ~f:sexp_of_t in
        List.fold ~init:base_sexp index_sexps ~f:(fun acc index_sexp ->
            let sexps = [Sexp.Atom "select"; acc; index_sexp] in
            Sexp.List sexps )
        (* let sexps = Sexp.Atom "select" :: base_sexp :: index_sexps in
         * Sexp.List sexps *)
    | ArrayStoreExpr {base; indices; value} ->
        let base_sexp = sexp_of_t base in
        let index_sexps = List.map indices ~f:sexp_of_t in
        let val_sexp = sexp_of_t value in
        let _, rev_list =
          List.fold index_sexps ~init:(base_sexp, [])
            ~f:(fun (base_sexp, acc) index_sexp ->
              let elem = (base_sexp, index_sexp) in
              let acc = elem :: acc in
              let base_sexp =
                Sexp.List [Sexp.Atom "select"; base_sexp; index_sexp]
              in
              (base_sexp, acc) )
        in
        List.fold rev_list ~init:val_sexp
          ~f:(fun acc (base_sexp, index_sexp) ->
            Sexp.List [Sexp.Atom "store"; base_sexp; index_sexp; acc] )
        (* let sexps =
         *   Sexp.Atom "store" :: base_sexp :: List.append index_sexps [val_sexp]
         * in
         * Sexp.List sexps *)
    | AnnotatedExpr {expr; _} -> sexp_of_t expr
    | CondExpr {cond; true_val; false_val} ->
        Sexp.List
          [ Sexp.Atom "ite"
          ; sexp_of_t cond
          ; sexp_of_t true_val
          ; sexp_of_t false_val ]
    | FunCallExpr {name; args} ->
        let name_sexp = Identifier.sexp_of_t name in
        let args_sexp = List.map args ~f:sexp_of_t in
        Sexp.List (name_sexp :: args_sexp)
    | QuantifiedExpr {quantifier; bindings; body} ->
        Sexp.List
          [ Quantifier.sexp_of_t quantifier
          ; Sexp.List (List.map bindings ~f:VarBinding.sexp_of_t)
          ; sexp_of_t body ]

  let rec t_of_sexp sexp =
    (* Try to parse as literal first *)
    try
      let lit = Literal.t_of_sexp sexp in
      LiteralExpr lit
    with Sexplib.Conv_error.Of_sexp_error _ -> (
      match sexp with
      | Sexp.Atom name ->
          let id = Identifier.of_string name in
          VarExpr id
      | Sexp.List [Sexp.Atom "-"; sexp'] ->
          let e = t_of_sexp sexp' in
          UnaryExpr (UnaryOperator.Neg, e)
      | Sexp.List [Sexp.Atom "not"; sexp'] ->
          let e = t_of_sexp sexp' in
          UnaryExpr (UnaryOperator.Not, e)
      | Sexp.List [Sexp.Atom "<"; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Lt, lhs, rhs)
      | Sexp.List [Sexp.Atom "<="; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Le, lhs, rhs)
      | Sexp.List [Sexp.Atom ">"; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Gt, lhs, rhs)
      | Sexp.List [Sexp.Atom ">="; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Ge, lhs, rhs)
      | Sexp.List [Sexp.Atom "=>"; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Imply, lhs, rhs)
      | Sexp.List [Sexp.Atom "="; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Eq, lhs, rhs)
      | Sexp.List [Sexp.Atom "mod"; lsexp; rsexp] ->
          let lhs = t_of_sexp lsexp in
          let rhs = t_of_sexp rsexp in
          BinaryExpr (BinaryOperator.Mod, lhs, rhs)
      | Sexp.List (Sexp.Atom "+" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.Plus, lhs, rhs) )
      | Sexp.List (Sexp.Atom "-" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.Minus, lhs, rhs) )
      | Sexp.List (Sexp.Atom "*" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.Mult, lhs, rhs) )
      | Sexp.List (Sexp.Atom "div" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.Div, lhs, rhs) )
      | Sexp.List (Sexp.Atom "and" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.And, lhs, rhs) )
      | Sexp.List (Sexp.Atom "or" :: rest_sexps)
        when not (List.is_empty rest_sexps) ->
          let exprs = List.map rest_sexps ~f:t_of_sexp in
          List.reduce_exn exprs ~f:(fun lhs rhs ->
              BinaryExpr (BinaryOperator.Or, lhs, rhs) )
      | Sexp.List [Sexp.Atom "ite"; cond_sexp; true_sexp; false_sexp] ->
          let cond = t_of_sexp cond_sexp in
          let true_val = t_of_sexp true_sexp in
          let false_val = t_of_sexp false_sexp in
          CondExpr {cond; true_val; false_val}
      | Sexp.List (Sexp.Atom "select" :: base_sexp :: index_sexps) ->
          let base = t_of_sexp base_sexp in
          let indices = List.map index_sexps ~f:t_of_sexp in
          ArraySelectExpr {base; indices}
      | Sexp.List [Sexp.Atom "forall"; Sexp.List binding_sexps; body_sexp] ->
          let bindings = List.map binding_sexps ~f:VarBinding.t_of_sexp in
          let body = t_of_sexp body_sexp in
          QuantifiedExpr {quantifier= Quantifier.ForAll; bindings; body}
      | Sexp.List [Sexp.Atom "exists"; Sexp.List binding_sexps; body_sexp] ->
          let bindings = List.map binding_sexps ~f:VarBinding.t_of_sexp in
          let body = t_of_sexp body_sexp in
          QuantifiedExpr {quantifier= Quantifier.Exists; bindings; body}
      | Sexp.List [Sexp.Atom "let"; Sexp.List binding_sexps; body_sexp] ->
          let subst_map =
            List.fold binding_sexps ~init:Identifier.Map.empty
              ~f:(fun acc sexps ->
                match sexps with
                | Sexp.List [Sexp.Atom name; esexp] ->
                    let id = Identifier.of_string name in
                    let e = t_of_sexp esexp in
                    Identifier.Map.set acc ~key:id ~data:e
                | _ as s ->
                    let msg = "Unrecognized let binding sexp" in
                    raise (Sexplib.Conv.Of_sexp_error (Failure msg, s)) )
          in
          let body = t_of_sexp body_sexp in
          subst subst_map body
      | Sexp.List (name_sexp :: args_sexp) ->
          let name = Identifier.t_of_sexp name_sexp in
          let args = List.map args_sexp ~f:t_of_sexp in
          FunCallExpr {name; args}
      | _ ->
          let msg = "Unrecognized expr sexp" in
          raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp)) )

  exception CannotResolveType

  let quick_type_of ?(ret_env = Identifier.Map.empty) ~var_env expr =
    let type_of_var vmap id =
      match Identifier.Map.find vmap id with
      | Some ty -> ty
      | _ -> raise CannotResolveType
    in
    let type_of_unary op _ =
      UnaryOperator.(
        match op with Neg -> Type.IntType | Not -> Type.BoolType)
    in
    let type_of_binary op _ _ =
      let open BinaryOperator in
      match op with
      | Plus | Minus | Mult | Div | Mod -> Type.IntType
      | And | Or | Imply | Lt | Le | Gt | Ge | Eq | Ne -> Type.BoolType
    in
    let rec run_expr vmap expr =
      match expr with
      | LiteralExpr lit -> Literal.type_of lit
      | VarExpr var -> type_of_var vmap var
      | UnaryExpr (op, e) ->
          let etype = run_expr vmap e in
          type_of_unary op etype
      | BinaryExpr (op, lhs, rhs) ->
          let ltype = run_expr vmap lhs in
          let rtype = run_expr vmap rhs in
          type_of_binary op ltype rtype
      | ArraySelectExpr {base; indices} -> (
        match run_expr vmap base with
        | Type.ArrayType (key_tys, val_ty) -> (
          match List.drop key_tys (List.length indices) with
          | [] -> val_ty
          | key_tys' -> Type.ArrayType (key_tys', val_ty) )
        | _ -> raise CannotResolveType )
      | ArrayStoreExpr {base; _} -> (
        match run_expr vmap base with
        | Type.ArrayType _ as ty -> ty
        | _ -> raise CannotResolveType )
      | AnnotatedExpr {expr; _} -> run_expr vmap expr
      | CondExpr {true_val; _} -> run_expr vmap true_val
      | FunCallExpr {name; _} -> (
        match Identifier.Map.find ret_env name with
        | Some ty -> ty
        | None -> raise CannotResolveType )
      | QuantifiedExpr {bindings; body; _} ->
          let vmap' =
            List.fold bindings ~init:vmap
              ~f:(fun acc VarBinding.({name; ty}) ->
                Identifier.Map.set acc name ty )
          in
          run_expr vmap' body
    in
    try
      let ty = run_expr var_env expr in
      Some ty
    with CannotResolveType -> None

  let quick_type_of_exn ?(ret_env = Identifier.Map.empty) ~var_env expr =
    match quick_type_of ~ret_env ~var_env expr with
    | Some ty -> ty
    | None -> raise (Not_found_s (Sexp.Atom "Expr.quick_type failed"))

  let pp_comma fmt () = Fmt.string fmt ", "

  let rec pp fmt = function
    | LiteralExpr l -> Literal.pp fmt l
    | VarExpr v -> Identifier.pp fmt v
    | UnaryExpr (op, e) -> Fmt.pf fmt "(%a %a)" UnaryOperator.pp op pp e
    | BinaryExpr (op, lhs, rhs) ->
        Fmt.pf fmt "(%a %a %a)" pp lhs BinaryOperator.pp op pp rhs
    | ArraySelectExpr {base; indices} ->
        Fmt.pf fmt "%a[%a]" pp base pp_comma_sep_list indices
    | ArrayStoreExpr {base; indices; value} ->
        Fmt.pf fmt "%a[%a = %a]" pp base pp_comma_sep_list indices pp value
    | AnnotatedExpr {annot; expr} -> Fmt.pf fmt "%a(%a)" Side.pp annot pp expr
    | CondExpr {cond; true_val; false_val} ->
        Fmt.pf fmt "(if %a then %a else %a)" pp cond pp true_val pp false_val
    | FunCallExpr {name; args} ->
        Fmt.pf fmt "%a(%a)" Identifier.pp name pp_comma_sep_list args
    | QuantifiedExpr {quantifier; bindings; body} ->
        let pp_bindings = Fmt.list VarBinding.pp ~sep:pp_comma in
        Fmt.pf fmt "(%a %a :: %a)" Quantifier.pp quantifier pp_bindings
          bindings pp body

  and pp_comma_sep_list fmt exprs = Fmt.list pp ~sep:pp_comma fmt exprs
end

module Lvalue = struct
  type t = {base: Identifier.t; indices: Expr.t list}
  [@@deriving sexp, compare, hash]

  let map_expr ~f lval =
    let indices = List.map lval.indices ~f in
    {lval with indices}

  let map_var ~f {base; indices} =
    let base = f base in
    let indices = List.map indices ~f:(Expr.map_var ~f) in
    {base; indices}

  let to_expr {base; indices} =
    match indices with
    | [] -> Expr.VarExpr base
    | _ -> Expr.ArraySelectExpr {base= Expr.VarExpr base; indices}

  let of_var base = {base; indices= []}

  let quick_type_of ~var_env lval =
    match lval.indices with
    | [] -> Identifier.Map.find var_env lval.base
    | _ -> Expr.quick_type_of ~var_env (to_expr lval)

  let quick_type_of_exn ~var_env lval =
    match quick_type_of ~var_env lval with
    | Some ty -> ty
    | None -> raise (Not_found_s (Sexp.Atom "Lvalue.quick_type failed"))

  let pp fmt {base; indices} =
    if List.is_empty indices then Identifier.pp fmt base
    else
      Fmt.pf fmt "%a" Expr.pp
        (Expr.ArraySelectExpr {base= Expr.VarExpr base; indices})
end

module Stmt = struct
  module ForDirection = struct
    type t = Forward | Backward [@@deriving sexp, compare, hash]

    let equal = [%compare.equal: t]
  end

  type t =
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

  let is_loop = function
    | While _ | For _ -> true
    | Assume _ | Assign _ | If _ | Call _ -> false

  let is_branch = function
    | If _ -> true
    | Assume _ | Assign _ | While _ | For _ | Call _ -> false

  let rec map_expr ~f = function
    | Assume e -> Assume (f e)
    | Assign {lhs; rhs} -> Assign {lhs= Lvalue.map_expr ~f lhs; rhs= f rhs}
    | If {cond; then_branch; else_branch} ->
        If
          { cond= f cond
          ; then_branch= List.map then_branch ~f:(map_expr ~f)
          ; else_branch= List.map else_branch ~f:(map_expr ~f) }
    | While {cond; body} ->
        While {cond= f cond; body= List.map body ~f:(map_expr ~f)}
    | For {counter; lower; upper; step; direction; body} ->
        For
          { counter
          ; lower= f lower
          ; upper= f upper
          ; step= f step
          ; direction
          ; body= List.map body ~f:(map_expr ~f) }
    | Call {rets; name; args} ->
        Call
          { rets= List.map ~f:(Lvalue.map_expr ~f) rets
          ; name
          ; args= List.map ~f args }

  let rec map_var ~f = function
    | Assume e -> Assume (Expr.map_var ~f e)
    | Assign {lhs; rhs} ->
        Assign {lhs= Lvalue.map_var ~f lhs; rhs= Expr.map_var ~f rhs}
    | If {cond; then_branch; else_branch} ->
        If
          { cond= Expr.map_var ~f cond
          ; then_branch= List.map then_branch ~f:(map_var ~f)
          ; else_branch= List.map else_branch ~f:(map_var ~f) }
    | While {cond; body} ->
        While {cond= Expr.map_var ~f cond; body= List.map body ~f:(map_var ~f)}
    | For {counter; lower; upper; step; direction; body} ->
        For
          { counter= f counter
          ; lower= Expr.map_var ~f lower
          ; upper= Expr.map_var ~f upper
          ; step= Expr.map_var ~f step
          ; direction
          ; body= List.map body ~f:(map_var ~f) }
    | Call {rets; name; args} ->
        Call
          { rets= List.map ~f:(Lvalue.map_var ~f) rets
          ; name
          ; args= List.map ~f:(Expr.map_var ~f) args }

  let rec read_vars_impl ~exclude_counter read_vars = function
    | Assume e -> Expr.free_vars_impl read_vars Identifier.Set.empty e
    | Assign {lhs= Lvalue.({indices; _}); rhs} ->
        List.iter indices
          ~f:(Expr.free_vars_impl read_vars Identifier.Set.empty) ;
        Expr.free_vars_impl read_vars Identifier.Set.empty rhs
    | If {cond; then_branch; else_branch} ->
        Expr.free_vars_impl read_vars Identifier.Set.empty cond ;
        read_vars_stmts_impl ~exclude_counter read_vars then_branch ;
        read_vars_stmts_impl ~exclude_counter read_vars else_branch
    | While {cond; body} ->
        Expr.free_vars_impl read_vars Identifier.Set.empty cond ;
        read_vars_stmts_impl ~exclude_counter read_vars body
    | For {counter; lower; upper; step; body; _} ->
        Expr.free_vars_impl read_vars Identifier.Set.empty lower ;
        Expr.free_vars_impl read_vars Identifier.Set.empty upper ;
        Expr.free_vars_impl read_vars Identifier.Set.empty step ;
        if not exclude_counter then Hash_set.add read_vars counter ;
        read_vars_stmts_impl ~exclude_counter read_vars body
    | Call {rets; args; _} ->
        List.iter rets ~f:(fun lval ->
            List.iter lval.indices
              ~f:(Expr.free_vars_impl read_vars Identifier.Set.empty) ) ;
        List.iter args ~f:(Expr.free_vars_impl read_vars Identifier.Set.empty)

  and read_vars_stmts_impl ~exclude_counter read_vars =
    List.iter ~f:(read_vars_impl ~exclude_counter read_vars)

  let read_var_set_of ?(exclude_counter = false) stmt =
    let read_vars = Identifier.Hash_set.create ~size:16 () in
    read_vars_impl ~exclude_counter read_vars stmt ;
    read_vars

  let rec write_vars_impl ~exclude_counter write_vars = function
    | Assume _ -> ()
    | Assign {lhs= Lvalue.({base; _}); _} -> Hash_set.add write_vars base
    | If {then_branch; else_branch; _} ->
        write_vars_stmts_impl ~exclude_counter write_vars then_branch ;
        write_vars_stmts_impl ~exclude_counter write_vars else_branch
    | While {body; _} -> write_vars_stmts_impl ~exclude_counter write_vars body
    | For {counter; body; _} ->
        if not exclude_counter then Hash_set.add write_vars counter ;
        write_vars_stmts_impl ~exclude_counter write_vars body
    | Call {rets; _} ->
        List.iter rets ~f:(fun lval -> Hash_set.add write_vars lval.base)

  and write_vars_stmts_impl ~exclude_counter write_vars =
    List.iter ~f:(write_vars_impl ~exclude_counter write_vars)

  let write_var_set_of ?(exclude_counter = false) stmt =
    let write_vars = Identifier.Hash_set.create ~size:16 () in
    write_vars_impl ~exclude_counter write_vars stmt ;
    write_vars

  let rec read_write_vars_impl ~exclude_counter rw_vars = function
    | Assume e -> Expr.free_vars_impl rw_vars Identifier.Set.empty e
    | Assign {lhs= Lvalue.({base; indices}); rhs} ->
        Hash_set.add rw_vars base ;
        List.iter indices ~f:(Expr.free_vars_impl rw_vars Identifier.Set.empty) ;
        Expr.free_vars_impl rw_vars Identifier.Set.empty rhs
    | If {cond; then_branch; else_branch} ->
        Expr.free_vars_impl rw_vars Identifier.Set.empty cond ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars then_branch ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars else_branch
    | While {cond; body} ->
        Expr.free_vars_impl rw_vars Identifier.Set.empty cond ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars body
    | For {counter; lower; upper; step; body; _} ->
        Expr.free_vars_impl rw_vars Identifier.Set.empty lower ;
        Expr.free_vars_impl rw_vars Identifier.Set.empty upper ;
        Expr.free_vars_impl rw_vars Identifier.Set.empty step ;
        if not exclude_counter then Hash_set.add rw_vars counter ;
        read_write_vars_stmts_impl ~exclude_counter rw_vars body
    | Call {rets; args; _} ->
        List.iter rets ~f:(fun lval ->
            Hash_set.add rw_vars lval.base ;
            List.iter lval.indices
              ~f:(Expr.free_vars_impl rw_vars Identifier.Set.empty) ) ;
        List.iter args ~f:(Expr.free_vars_impl rw_vars Identifier.Set.empty)

  and read_write_vars_stmts_impl ~exclude_counter rw_vars =
    List.iter ~f:(read_write_vars_impl ~exclude_counter rw_vars)

  let read_write_var_set_of ?(exclude_counter = false) stmt =
    let rw_vars = Identifier.Hash_set.create ~size:32 () in
    read_write_vars_impl ~exclude_counter rw_vars stmt ;
    rw_vars

  let pp_for_step fmt = function
    | Expr.LiteralExpr (Literal.IntLit i) when Bigint.equal i Bigint.one -> ()
    | _ as e -> Fmt.pf fmt " step %a" Expr.pp e

  let pp_direction fmt = function
    | ForDirection.Forward -> Fmt.pf fmt "to"
    | ForDirection.Backward -> Fmt.pf fmt "downto"

  let rec pp fmt = function
    | Assume expr -> Fmt.pf fmt "assume(%a);" Expr.pp expr
    | Assign {lhs; rhs} -> Fmt.pf fmt "%a = %a;" Lvalue.pp lhs Expr.pp rhs
    | If {cond; then_branch; else_branch} ->
        let pp_else_stmts fmt stmts =
          if List.is_empty stmts then ()
          else Fmt.pf fmt " else { %a }" pp_stmts else_branch
        in
        Fmt.pf fmt "if (%a) { %a }%a" Expr.pp cond pp_stmts then_branch
          pp_else_stmts else_branch
    | While {cond; body} ->
        Fmt.pf fmt "while (%a) { %a }" Expr.pp cond pp_stmts body
    | For {counter; lower; upper; step; direction; body} ->
        Fmt.pf fmt "for (%a = %a %a %a%a) { %a }" Identifier.pp counter Expr.pp
          lower pp_direction direction Expr.pp upper pp_for_step step pp_stmts
          body
    | Call {rets; name; args} ->
        Fmt.pf fmt "call %a = %a(%a)"
          (Fmt.list ~sep:Fmt.comma Lvalue.pp)
          rets Identifier.pp name
          (Fmt.list ~sep:Fmt.comma Expr.pp)
          args

  and pp_stmts fmt stmts = Fmt.list pp ~sep:Fmt.sp fmt stmts

  let rec pp_brief fmt = function
    | Assume expr -> Fmt.pf fmt "assume(%a);" Expr.pp expr
    | Assign {lhs; rhs} -> Fmt.pf fmt "%a = %a;" Lvalue.pp lhs Expr.pp rhs
    | If {cond; then_branch; else_branch} ->
        let pp_else_stmts fmt stmts =
          if List.is_empty stmts then ()
          else Fmt.pf fmt "else %a" pp_stmts_brief else_branch
        in
        Fmt.pf fmt "if (%a) %a %a" Expr.pp cond pp_stmts_brief then_branch
          pp_else_stmts else_branch
    | While {cond; body} ->
        Fmt.pf fmt "while (%a) %a" Expr.pp cond pp_stmts_brief body
    | For {counter; lower; upper; step; direction; body} ->
        Fmt.pf fmt "for (%a = %a %a %a%a) %a" Identifier.pp counter Expr.pp
          lower pp_direction direction Expr.pp upper pp_for_step step
          pp_stmts_brief body
    | Call {rets; name; args} ->
        Fmt.pf fmt "call %a = %a(%a)"
          (Fmt.list ~sep:Fmt.comma Lvalue.pp)
          rets Identifier.pp name
          (Fmt.list ~sep:Fmt.comma Expr.pp)
          args

  and pp_stmts_brief fmt = function
    | [] -> Fmt.string fmt "{}"
    | [stmt] -> pp_brief fmt stmt
    | stmt :: rest ->
        let rest_len = List.length rest in
        Fmt.pf fmt "{ %a; ... (%d more) }" pp_brief stmt rest_len
end

module Stmts = struct
  type t = Stmt.t list

  let map_var ~f = List.map ~f:(Stmt.map_var ~f)

  let map_expr ~f = List.map ~f:(Stmt.map_expr ~f)

  let read_var_set_of ?(exclude_counter = false) stmts =
    let read_vars = Identifier.Hash_set.create ~size:64 () in
    Stmt.read_vars_stmts_impl ~exclude_counter read_vars stmts ;
    read_vars

  let write_var_set_of ?(exclude_counter = false) stmts =
    let write_vars = Identifier.Hash_set.create ~size:64 () in
    Stmt.write_vars_stmts_impl ~exclude_counter write_vars stmts ;
    write_vars

  let read_write_var_set_of ?(exclude_counter = false) stmts =
    let rw_vars = Identifier.Hash_set.create ~size:128 () in
    Stmt.read_write_vars_stmts_impl ~exclude_counter rw_vars stmts ;
    rw_vars

  let pp = Stmt.pp_stmts

  let pp_brief = Stmt.pp_stmts_brief
end

module FunDecl = struct
  module T = struct
    type t = {name: Identifier.t; param_tys: Type.t list; ret_ty: Type.t}
    [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module Procedure = struct
  type t =
    { name: Identifier.t
    ; params: VarBinding.t list
    ; rets: VarBinding.t list
    ; locals: VarBinding.t list
    ; stmts: Stmt.t list }
  [@@deriving sexp, compare, hash]
end

module EntrySpec = struct
  type t = {left: Identifier.t; right: Identifier.t}
  [@@deriving sexp, compare, hash]

  let default_left = Identifier.of_string "f0"

  let default_right = Identifier.of_string "f1"

  let default = {left= default_left; right= default_right}
end

module Spec = struct
  type t = {requires: Expr.t list; ensures: Expr.t list}
  [@@deriving sexp, compare, hash]
end

type t =
  { decls: FunDecl.t list
  ; procs: Procedure.t list
  ; entry: EntrySpec.t
  ; spec: Spec.t }
[@@deriving sexp, compare, hash]

let lookup_decl {decls; _} id =
  List.find decls ~f:(fun decl -> Identifier.equal id decl.FunDecl.name)

let lookup_decl_exn prog id =
  match lookup_decl prog id with
  | Some decl -> decl
  | None ->
      let msg =
        Fmt.strf "Coeus declaration lookup failed: %a" Identifier.pp id
      in
      raise (Not_found_s (Sexp.Atom msg))

let lookup_proc {procs; _} id =
  List.find procs ~f:(fun proc -> Identifier.equal id proc.Procedure.name)

let lookup_proc_exn prog id =
  match lookup_proc prog id with
  | Some proc -> proc
  | None ->
      let msg =
        Fmt.strf "Coeus procedure lookup failed: %a" Identifier.pp id
      in
      raise (Not_found_s (Sexp.Atom msg))

let lookup_entry_proc prog = function
  | Side.Left -> lookup_proc prog prog.entry.left
  | Side.Right -> lookup_proc prog prog.entry.right

let lookup_entry_proc_exn prog side =
  match lookup_entry_proc prog side with
  | Some proc -> proc
  | None ->
      let msg =
        Fmt.strf "Coeus %a-entry procedure lookup failed" Side.pp side
      in
      raise (Not_found_s (Sexp.Atom msg))

let spec_map prog ~f =
  let spec : Spec.t = prog.spec in
  let requires = List.map ~f spec.requires in
  let ensures = List.map ~f spec.ensures in
  {prog with spec= Spec.{requires; ensures}}

let proc_map prog ~f =
  let procs = List.map ~f prog.procs in
  {prog with procs}

let update_proc prog proc =
  let id = proc.Procedure.name in
  let procs' =
    List.filter prog.procs ~f:(fun proc ->
        not (Identifier.equal id proc.Procedure.name) )
  in
  let procs = proc :: procs' in
  {prog with procs}
