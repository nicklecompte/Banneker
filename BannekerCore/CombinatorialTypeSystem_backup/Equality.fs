module CombinatorialTypeSystem.Equality

open CombinatorialTypeSystem
open BasicTypes
open Term

let rec syntacticallyEqualTerms termA termB : bool =
    match (termA,termB) with
    | (LitConstant a, LitConstant b) -> a = b
    | (PrimFn a, PrimFn b) -> a = b
    | (SortTerm a, SortTerm b) -> a = b
    | (Variable(TypedVar(nA,tA,bA)),Variable(TypedVar(nB,tB,bB))) ->
        failwith "not done"
    | _ -> false

let rec alphaEquivalentTerms termA termB =
    match (termA,termB) with
    | (LitConstant a,LitConstant b) -> a = b
    | (PrimFn a, PrimFn b) -> a = b
    | (SortTerm (Sort.Simple stA), SortTerm (Sort.Simple stB)) -> stA = stB
    | (SortTerm (Sort.Inductive(stA,dA)), SortTerm (Sort.Inductive(stB,dB))) -> stA = stB && dA = dB
    | ((Variable (TypedVar(a,_,_))),Variable (TypedVar(b,_,_))) ->
        // Alpha-equivalence is just about renaming
        // hence we can drop the type and check the names
        a = b
    | ((LambdaAbstraction(na,tA,vA),LambdaAbstraction(nb,tB,vB))) -> 
        let aEquivalentTypes = alphaEquivalentTerms tA tB
        if aEquivalentTypes then
            let renamed = renameVariableInTerm vB nb na
            alphaEquivalentTerms vA renamed
        else false
    | ((Application (tA,sA)),(Application (tB,sB))) ->
        (alphaEquivalentTerms tA tB) && (alphaEquivalentTerms sA sB)
    | _ -> false

let rec betaEquivalentTerms termA termB =
    match (termA,termB) with
    | (SortTerm stA, SortTerm stB) -> stA = stB
    | ((Variable (TypedVar(a,_,_))),Variable (TypedVar(b,_,_))) ->
        // Beta-equivalence is just about renaming
        // hence we can drop the type and check the names
        a = b