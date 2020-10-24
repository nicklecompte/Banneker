/// This module essentially defines the "meat" of Banneker's snytax and typechecking.
module Banneker.PureTypeSystem

type Name =
    | UserName of string
    | MachineGenName of string * int

type Sort = Sort of string
with
    member x.StringVal = match x with | Sort s -> s


type PureTypeSystem = {
    sorts : Sort list 
    axioms : (Sort*Sort) list
    rules : (Sort*Sort*Sort) list
}
with
    member x.IsValid =
        let sortsFromAxioms = List.concat [(List.map fst (x.axioms)); (List.map snd (x.axioms))] |> List.distinct
        let sortsFromRules = 
            List.concat [
                (List.map (fun (x,_,_) -> x) (x.rules));
                (List.map (fun (_,x,_) -> x) (x.rules));
                (List.map (fun (_,_,x) -> x) (x.rules))
            ]
            |> List.distinct
        for s in sortsFromAxioms do
            if not (List.contains s (x.sorts)) then 
                (failwith (sprintf "Sort %s was in the axiom set but not in the sort set" (s.StringVal)))
        for s in sortsFromRules do
            if not (List.contains s (x.sorts)) then 
                (failwith (sprintf "Sort %s was in the rules set but not in the sort set" (s.StringVal)))
        true

let inline ptsContainsAxiom pts (s1,s2) : bool =
    List.contains (s1,s2) (pts.axioms)

type Term = 
    | SortTerm of Sort
    | Variable of TypedVar
    | LambdaAbstraction of argumentName:Name*argumentType:Term * varBody : Term
    | Application of Term*Term
    | PiAbstraction of piArgumentName:Name*piArgumentType:Term * piVarBody : Term

and TypedVar = 
    | TypedVar of Name*Term
    | AxiomVar of Sort*Sort

type Signature =
    | Data of Term
    | Function of Signature*Signature

let rec getSignatureOfTerm term =
    match term with
    | SortTerm _ -> Some (Data term)
    | Variable(TypedVar(_,t)) -> Some (Data t)
    | Variable(AxiomVar(_,s)) -> Some (Data (SortTerm s))
    | LambdaAbstraction(a,t,b) ->
        let bodySig = getSignatureOfTerm b
        let argSig = getSignatureOfTerm t
        match (argSig,bodySig) with
        | (Some aS,Some bS) ->
           Some (Function(aS,bS))
        | _ -> None
    | Application(tA,tB) ->
        let aSig = getSignatureOfTerm tA
        let bSig = getSignatureOfTerm tB
        match aSig with
        | Some (Function(a,b)) ->
            if bSig = (Some a) then Some b
            else None
        | _ -> None

let rec renameVariableInTerm term oldName newName =
    match term with
    | SortTerm _ -> term
    | Variable(AxiomVar(_,_)) -> term
    | Variable(TypedVar(n,t)) -> 
        if n = oldName then Variable(TypedVar(newName,t)) else term
    | LambdaAbstraction(n,t,b) ->
        let name = if n = oldName then newName else n
        LambdaAbstraction(name,t,(renameVariableInTerm b oldName newName))
    | Application(tA,tB) -> 
        Application((renameVariableInTerm tA oldName newName),
                    (renameVariableInTerm tB oldName newName))
    | PiAbstraction(p,pt,pb) ->
        let name = if p = oldName then newName else p
        PiAbstraction(name,pt,pb)
    

let rec alphaEquivalentTerms termA termB =
    match (termA,termB) with
    | (SortTerm (Sort stA), SortTerm (Sort stB)) -> stA = stB
    | ((Variable (TypedVar(_,tA))),Variable (TypedVar(_,tB))) ->
        // Alpha-equivalence is just about renaming
        // hence we can drop the name and check alpha-equivalence of the types
        alphaEquivalentTerms tA tB
    | ((LambdaAbstraction(_,tA,vA),LambdaAbstraction(_,tB,vB))) -> 
        let alphaEquivalentArguments = alphaEquivalentTerms tA tB
        if alphaEquivalentArguments then (alphaEquivalentTerms vA vB) else false
    | ((Application (tA,sA)),(Application (tB,sB))) ->
        (alphaEquivalentTerms tA tB) && (alphaEquivalentTerms sA sB)
    | ((PiAbstraction(_,tA,vA),PiAbstraction(_,tB,vB))) -> 
        let alphaEquivalentArguments = alphaEquivalentTerms tA tB
        if alphaEquivalentArguments then (alphaEquivalentTerms vA vB) else false

let rec evalutateTerm term =
    match term with
    | (Application(LambdaAbstraction(n,tA,vA),tB)) -> 
        failwithf "not done"
    | _ -> term

type Context = {
    pts : PureTypeSystem
    vars : TypedVar list
}

type CheckedContext =
    | VerifiedCtx of Context
    | ErrorCtx of string

let rec checkIfWellTyped (typedVar: TypedVar) (context: Context) : CheckedContext =
    match typedVar with
    // Axiom verification comes directly from PTS object in Context
    | AxiomVar(s1,s2) -> 
        // just chekc if it's in the axiom list
        match ptsContainsAxiom (context.pts) (s1,s2) with
        | true -> VerifiedCtx context
        | false -> 
            // Extra branching here only for debugging purposes.
            match List.contains s1 (context.pts.sorts) with
            | true ->
                if List.exists (fun (x1,_) -> s1 = x1) (context.pts.axioms) then
                    ErrorCtx (sprintf "The sort %s does not exist in the context." (s1.StringVal))
                    // TODO: Fill this in!
                    // Need to check s2 doens't exist in context sorts and s1:s2 not in context rulez
                    else ErrorCtx ("")

            | false ->
                // TODO: Fill this in! 
                ErrorCtx ("")
    | TypedVar(_,t) -> // don't care about the name
        match t with
        | SortTerm s -> match List.contains s (context.pts.sorts) with
                        | true -> VerifiedCtx context
                        | false -> ErrorCtx (sprintf "The sort %s does not exist in the context." (s.StringVal))
        // ex t = Type : Kind
        | Variable (AxiomVar(s1,s2)) -> 
            // We just need to check the well-typedness of the axiomvar term itself
            checkIfWellTyped (AxiomVar(s1,s2)) context
        // Ex, t = Nat : Type or List : Type -> Type
        | Variable (TypedVar(x,typeTerm)) ->
            match typeTerm with
            | Variable (TypedVar(t,(SortTerm s))) ->
                checkIfWellTyped (TypedVar(t,(SortTerm s))) context

        | LambdaAbstraction(n,typ,b) -> failwith "not done"
        | Application (t1,t2) -> failwith "not done"
        | PiAbstraction(n,typ,b) -> failwith "not done"
        //| _ -> ErrorCtx("")