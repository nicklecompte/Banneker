module CombinatorialTypeSystem.Signature

/// Helper for getting the signatures of a built-in within a type system.
/// typeType is the Sort representing the "type of Types" - whichever sort
/// it is sensible to put Literal types into.
/// E.g. we might want to try something like Int : Primitive, Primitive : Type_1, along with Type = Type_0.
let getSignatureOfLiteral lit (typeType:Sort) =
    
    let intType = Variable (TypedVar(UserName "Int",SortTerm typeType,Empty))
    match lit with
    | Int i -> Data intType

let rec getSignatureOfTerm term =
    match term with
    | LitConstant _ -> None
    | PrimFn _ -> None
    | SortTerm _ -> Some term
    | Variable(TypedVar(_,t,_)) -> Some t
    | Variable(AxiomVar(_,s)) -> Some (SortTerm s)
    | DataConstructor(_,t,tt) -> Some (Application(t,tt))
    | LambdaAbstraction(_,t,b) ->
        let bodySig = getSignatureOfTerm b
        let argSig = getSignatureOfTerm t
        match (argSig,bodySig) with
        | (Some aS,Some bS) ->
           Some (Application(aS,bS))
        | _ -> None