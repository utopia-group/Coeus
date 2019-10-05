%{
open Coeus
open Core

%}

%token T_EOF

/* Keywords */
%token
  T_ASSUME T_BOOL T_ELSE T_ENSURES T_EXISTS T_FALSE T_FORALL
  T_IF T_THEN T_INT T_PROCEDURE T_DECLARE T_REQUIRES T_RETURNS T_TRUE
  T_WHILE T_FOR T_TO T_DOWNTO T_STEP T_CALL
  T_LEFT_ANNOT T_RIGHT_ANNOT T_LEFT_ENTRY T_RIGHT_ENTRY

/* Operators */
%token
  T_PLUS T_MINUS T_TIMES T_DIV T_MOD T_AND T_OR T_IMPLIES T_EQ T_NE T_LT T_LE T_GT T_GE
  T_PLUSEQ T_MINUSEQ T_TIMESEQ T_DIVEQ T_MODEQ T_PLUSPLUS T_MINUSMINUS
  T_NEG T_NOT
  T_DCOLON T_COLON T_ASSIGN T_COMMA T_SEMI

/* Parenthesis */
%token
  T_LP T_RP T_LC T_RC T_LB T_RB

/* Literals */
%token <Coeus.Identifier.t> T_IDENT
%token <Bigint.t> T_NUMBER

/* Precedence and associativity */
%nonassoc T_DCOLON
%right T_IMPLIES
%left T_OR
%left T_AND
%nonassoc T_EQ T_NE T_LT T_LE T_GT T_GE 
%left T_PLUS T_MINUS 
%left T_TIMES T_DIV T_MOD 
%right T_NEG T_NOT 
%nonassoc T_LB

%nonassoc NO_ELSE
%nonassoc T_ELSE

%start <Coeus.t> main

%%

main:
  | list(fun_decl) list(proc_decl) entry_spec specs T_EOF {
    { decls=$1; procs=$2; entry=$3; spec=$4 }
  }
  
fun_decl:
  | T_DECLARE type_name T_IDENT delimited(T_LP, separated_list(T_COMMA, type_name), T_RP) T_SEMI {
    FunDecl.{ name = $3; param_tys = $4; ret_ty = $2 }
  }

proc_decl:
  | T_PROCEDURE T_IDENT delimited(T_LP, param_list, T_RP) proc_rets proc_body  {
    let (locals, stmts) = $5 in
    Procedure.{ name=$2; params=$3; rets=$4; locals; stmts }
  }

proc_rets:
  | loption(preceded(T_RETURNS, delimited(T_LP, param_list, T_RP))) { $1 }

param_list:
  | separated_list(T_COMMA, param_decl)  { $1 }

nonempty_param_list:
  | separated_nonempty_list(T_COMMA, param_decl)  { $1 }
  
param_decl:
  | type_name T_IDENT { VarBinding.{ name=$2; ty=$1 } }

typed_idents:
  | type_name separated_nonempty_list(T_COMMA, T_IDENT) {
    ($2, $1)
  }

type_name:
  | T_BOOL               { Type.BoolType }
  | T_INT                { Type.IntType }
  | type_name T_LB separated_nonempty_list(T_COMMA, type_name) T_RB  { Type.ArrayType ($3, $1) }

proc_body:
  | delimited(T_LC, proc_body_content, T_RC) { $1 }
  
proc_body_content:
  | local_vars list(stmt) {
    ($1, $2)
  }

local_vars: 
  | list(local_var)  { List.concat $1 }

local_var:
  | typed_idents T_SEMI {
    let (names, ty) = $1 in
    List.map names ~f:(fun name -> VarBinding.{ name; ty })
  }
  
stmt:
  | T_ASSUME T_LP expr T_RP T_SEMI                       { Stmt.Assume $3 }
  | lval T_ASSIGN expr T_SEMI                            { Stmt.Assign { lhs=$1; rhs=$3} }
  | lval T_PLUSEQ expr T_SEMI                            {
    let rhs = Expr.BinaryExpr (BinaryOperator.Plus, Lvalue.to_expr $1, $3) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | lval T_MINUSEQ expr T_SEMI                           {
    let rhs = Expr.BinaryExpr (BinaryOperator.Minus, Lvalue.to_expr $1, $3) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | lval T_TIMESEQ expr T_SEMI                           {
    let rhs = Expr.BinaryExpr (BinaryOperator.Mult, Lvalue.to_expr $1, $3) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | lval T_DIVEQ expr T_SEMI                             {
    let rhs = Expr.BinaryExpr (BinaryOperator.Div, Lvalue.to_expr $1, $3) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | lval T_MODEQ expr T_SEMI                             {
    let rhs = Expr.BinaryExpr (BinaryOperator.Mod, Lvalue.to_expr $1, $3) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | lval T_PLUSPLUS T_SEMI                               {
    let one = Expr.LiteralExpr (Literal.IntLit Bigint.one) in
    let rhs = Expr.BinaryExpr (BinaryOperator.Plus, Lvalue.to_expr $1, one) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | T_PLUSPLUS lval T_SEMI                               {
    let one = Expr.LiteralExpr (Literal.IntLit Bigint.one) in
    let rhs = Expr.BinaryExpr (BinaryOperator.Plus, Lvalue.to_expr $2, one) in
    Stmt.Assign { lhs=$2; rhs}
  }
  | lval T_MINUSMINUS T_SEMI                             {
    let one = Expr.LiteralExpr (Literal.IntLit Bigint.one) in
    let rhs = Expr.BinaryExpr (BinaryOperator.Minus, Lvalue.to_expr $1, one) in
    Stmt.Assign { lhs=$1; rhs}
  }
  | T_MINUSMINUS lval T_SEMI                             {
    let one = Expr.LiteralExpr (Literal.IntLit Bigint.one) in
    let rhs = Expr.BinaryExpr (BinaryOperator.Minus, Lvalue.to_expr $2, one) in
    Stmt.Assign { lhs=$2; rhs}
  }
  | T_IF delimited(T_LP, expr, T_RP) block %prec NO_ELSE {
    Stmt.If { cond=$2; then_branch=$3; else_branch=[] }
  }
  | T_CALL separated_list(T_COMMA, lval) T_ASSIGN T_IDENT delimited(T_LP, separated_list(T_COMMA, expr), T_RP) T_SEMI {
    Stmt.Call { rets=$2; name=$4; args=$5 }
  }
  | T_IF delimited(T_LP, expr, T_RP) block T_ELSE block  {
    Stmt.If { cond=$2; then_branch=$3; else_branch=$5 }
  }
  | T_WHILE delimited(T_LP, expr, T_RP) block            { Stmt.While { cond=$2; body=$3 } }
  | T_FOR T_LP T_IDENT T_ASSIGN expr for_direction expr for_step T_RP block {
    Stmt.For { counter=$3; lower=$5; upper=$7; step=$8; direction=$6; body=$10}
  }
  
for_direction:
  | T_TO      { Stmt.ForDirection.Forward }
  | T_DOWNTO  { Stmt.ForDirection.Backward }
  
for_step:
  | option(preceded(T_STEP, expr)) {
    Option.value $1 ~default:(Expr.LiteralExpr (Literal.IntLit Bigint.one))
  }
  
block:
  | stmt                               { [$1] }
  | delimited(T_LC, list(stmt), T_RC)  { $1 }
  
lval:
  | T_IDENT {
    Lvalue.{ base=$1; indices=[] }
  }
  | T_IDENT delimited(T_LB, separated_list(T_COMMA, expr), T_RB) {
    Lvalue.{ base=$1; indices=$2 }
  }

expr:
  | literal {
    Expr.LiteralExpr $1
  }
  | T_IDENT {
    Expr.VarExpr $1
  }
  | T_NOT expr {
    Expr.UnaryExpr (UnaryOperator.Not, $2)
  }
  | T_MINUS expr %prec T_NEG {
    Expr.UnaryExpr (UnaryOperator.Neg, $2)
  }
  | expr binary_op expr {
    Expr.BinaryExpr ($2, $1, $3)
  }
  | expr_annot delimited(T_LP, expr, T_RP) {
    Expr.AnnotatedExpr { annot=$1; expr=$2 }
  }
  | T_IF expr T_THEN expr T_ELSE expr %prec T_NEG {
    Expr.CondExpr { cond = $2; true_val = $4; false_val = $6 }
  }
  | expr delimited(T_LB, separated_nonempty_list(T_COMMA, expr), T_RB) {
    Expr.ArraySelectExpr { base = $1; indices = $2 }
  }
  | expr T_LB separated_nonempty_list(T_COMMA, expr) T_ASSIGN expr T_RB {
    Expr.ArrayStoreExpr { base = $1; indices = $3; value = $5 }
  }
  | T_IDENT delimited(T_LP, separated_list(T_COMMA, expr), T_RP) {
    Expr.FunCallExpr { name = $1; args = $2 }
  }
  | quantifier nonempty_param_list T_DCOLON expr {
    Expr.QuantifiedExpr { quantifier = $1; bindings = $2; body = $4 }
  }
  | delimited(T_LP, expr, T_RP) {
    $1
  }
  
expr_annot:
  | T_LEFT_ANNOT   { Side.Left }
  | T_RIGHT_ANNOT  { Side.Right }
  
literal:
  | T_TRUE    { Literal.BoolLit true }
  | T_FALSE   { Literal.BoolLit false }
  | T_NUMBER  { Literal.IntLit $1 }

%inline binary_op:
  | T_PLUS     { BinaryOperator.Plus }
  | T_MINUS    { BinaryOperator.Minus }
  | T_TIMES    { BinaryOperator.Mult }
  | T_DIV      { BinaryOperator.Div }
  | T_MOD      { BinaryOperator.Mod }
  | T_AND      { BinaryOperator.And }
  | T_OR       { BinaryOperator.Or }
  | T_IMPLIES  { BinaryOperator.Imply }
  | T_LT       { BinaryOperator.Lt }
  | T_LE       { BinaryOperator.Le }
  | T_GT       { BinaryOperator.Gt }
  | T_GE       { BinaryOperator.Ge }
  | T_EQ       { BinaryOperator.Eq }
  | T_NE       { BinaryOperator.Ne }

%inline quantifier:
  | T_FORALL  { Quantifier.ForAll }
  | T_EXISTS  { Quantifier.Exists }
  
entry_spec:
  | option(lentry_spec) option(rentry_spec) {
    let left = Option.value $1 ~default:EntrySpec.default_left in
    let right = Option.value $2 ~default:EntrySpec.default_right in
    EntrySpec.{ left; right }
  }

lentry_spec:
  | T_LEFT_ENTRY T_IDENT T_SEMI { $2 }

rentry_spec:
  | T_RIGHT_ENTRY T_IDENT T_SEMI { $2 }

specs:
  | list(require_spec) list(ensure_spec) {
    Spec.{ requires=$1; ensures=$2 }
  }

require_spec:
  | T_REQUIRES expr T_SEMI { $2 }

ensure_spec:
  | T_ENSURES expr T_SEMI { $2 }
