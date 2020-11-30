module CombinatorialTypeSystem.TypeChecker

open CombinatorialTypeSystem.Term

type CombinatorialTypeSystem = | Em

type Context = {
    pts : CombinatorialTypeSystem
    vars : TypedVar list
}

// let isVariableInContext typedVar context =
//     List.contains (context.vars) typedVar

// type CheckedContext =
//     | VerifiedCtx of Context
//     | ErrorCtx of string

// let rec checkIfWellTyped (typedVar: TypedVar) (context: Context) : CheckedContext =
//     match typedVar with
//     // Axiom verification comes directly from PTS object in Context
//     | AxiomVar(s1,s2) -> 
//         // just chekc if it's in the axiom list
//         match ptsContainsAxiom (context.pts) (s1,s2) with
//         | true -> VerifiedCtx context
//         | false -> 
//             // Extra branching here only for debugging purposes.
//             match Seq.contains s1 (context.pts.sorts) with
//             | true ->
//                 if Seq.exists (fun (x1,_) -> s1 = x1) (context.pts.axioms) then
//                     ErrorCtx (sprintf "The sort %s does not exist in the context." (s1.StringVal))
//                     // TODO: Fill this in!
//                     // Need to check s2 doens't exist in context sorts and s1:s2 not in context rulez
//                     else ErrorCtx ("")

//             | false ->
//                 // TODO: Fill this in! 
//                 ErrorCtx ("")
//     | TypedVar(_,t) -> // don't care about the name
//         match t with
//         | PrimFn _ -> failwith "not done"
//         | SortTerm s -> match Seq.contains s (context.pts.sorts) with
//                         | true -> VerifiedCtx context
//                         | false -> ErrorCtx (sprintf "The sort %s does not exist in the context." (s.StringVal))
//         // ex t = Type : Kind
//         | Variable (AxiomVar(s1,s2)) -> 
//             // We just need to check the well-typedness of the axiomvar term itself
//             checkIfWellTyped (AxiomVar(s1,s2)) context
//         // Ex, t = Nat : Type or List : Type -> Type
//         | Variable (TypedVar(x,typeTerm)) ->
//             match typeTerm with
//             | Variable (TypedVar(t,(SortTerm s))) ->
//                 checkIfWellTyped (TypedVar(t,(SortTerm s))) context

//         | LambdaAbstraction(n,typ,b) -> failwith "not done"
//         | Application (t1,t2) -> failwith "not done"
//         //| _ -> ErrorCtx("")