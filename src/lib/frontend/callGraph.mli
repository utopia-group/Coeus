include Graph.Sig.P
        with type V.t = Ast.Coeus.Identifier.t
         and type V.label = Ast.Coeus.Identifier.t
         and type E.t = Ast.Coeus.Identifier.t * Ast.Coeus.Identifier.t
         and type E.label = unit

module Builder : sig
  val of_coeus : Ast.Coeus.t -> t

  val of_ecoeus : Ast.Ecoeus.t -> t
end
