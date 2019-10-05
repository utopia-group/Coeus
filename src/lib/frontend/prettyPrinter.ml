open Core
open Ast.Coeus

let run prog =
  Ast.CoeusPrettyPrint.pp Fmt.stdout prog ;
  Result.Ok prog
