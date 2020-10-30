/// This module essentially defines the "meat" of Banneker's snytax and typechecking.
module PureTypeSystem
open BuiltinPrimitives
open Utils

type Name =
    | UserName of string
    | MachineGenName of string * int

type Sort = 
    | Simple of string
    // For Calculus of Inductive Constructions, etc
    // This automatically defines an axiom in a PTS:
    // Given s = InductiveFamily(t), then (s,t) \in \mathcal{R}
    | Inductive of root:string*depth:int
with
    member inline x.StringVal = 
        match x with 
            | Simple s -> s
            | Inductive(s,i) -> sprintf "%s_%i" s i
       //     | InductiveFamily f

type PTSAxiom =
    | Simple of Sort*Sort
    | Inductive of root:string*lowToHigh:(int -> int)

type PTSRule =
    | Simple of Sort*Sort
    | Inductive of root: string * map: (int -> int -> int)

type PureTypeSystem = {
    sorts : Sort list 
    axioms : PTSAxiom list
    rules : PTSRule List
}

type ValidatedPTS =
    | ValidPTS of PureTypeSystem
    | InvalidPTS of string

// let createPureTypeSystem (sorts:Sort list) (axioms:(Sort*Sort) list) (rules:(Sort*Sort*Sort) list) : ValidatedPTS =
//     let sortsFromAxioms = List.concat [(List.map fst (axioms)); (List.map snd (axioms))] |> Seq.distinct
//     let sortsFromRules = 
//         List.concat [
//             (List.map (fun (x,_,_) -> x) (rules));
//             (List.map (fun (_,x,_) -> x) (rules));
//             (List.map (fun (_,_,x) -> x) (rules))
//         ]
//         |> Seq.distinct
//     match List.tryFind (fun s -> not (Seq.contains s sorts)) sortsFromAxioms with
//     | Some s -> InvalidPTS (sprintf "Sort %s was in the axiom set but not in the sort set" (s.StringVal))
//     | None ->
//         match Seq.tryFind (fun s -> not (Seq.contains s sorts)) sortsFromRules with
//         | Some s -> InvalidPTS (sprintf "Sort %s was in the rules set but not in the sort set" (s.StringVal))
//         | None -> ValidPTS {sorts=sorts;axioms=axioms;rules=rules}
    

// let inline ptsContainsAxiom pts (s1,s2) : bool =
//     Seq.contains (s1,s2) (pts.axioms)

type Term = 
    // LitConstant and PrimFn exist metatheoretically. 
    | LitConstant of Literal
    | PrimFn of PrimitiveFunction
    | SortTerm of Sort
    | Variable of TypedVar
    | LambdaAbstraction of argumentName:Name*argumentType:Term * varBody : Term
    | Application of Term*Term
    // TODO: 
    // There is definitely a "combinatorial homotopy" here linking the structure of the sorts
    // and eta-reducton along Pi vs. Lambda abstractions.
    // | PiAbstraction of piArgumentName:Name*piArgumentType:Term * piVarBody : Term

and TypedVar = 
    | TypedVar of Name*Term
    | AxiomVar of Sort*Sort
with
    member x.Name = 
        match x with
        | TypedVar(n,_) -> n
        | AxiomVar(s,_) -> UserName (s.StringVal)

type Signature =
    | Data of Term
    | Function of Signature*Signature

let getSignatureOfLiteral lit (typeType:Sort) =
    let intType = Variable (TypedVar(UserName "Int",SortTerm typeType))
    match lit with
    | Int i -> Data intType

let rec getSignatureOfTerm term =
    match term with
    | LitConstant _ -> None
    | PrimFn _ -> None
    | SortTerm _ -> Some (Data term)
    | Variable(TypedVar(_,t)) -> Some (Data t)
    | Variable(AxiomVar(_,s)) -> Some (Data (SortTerm s))
    | LambdaAbstraction(_,t,b) ->
        let bodySig = getSignatureOfTerm b
        let argSig = getSignatureOfTerm t
        match (argSig,bodySig) with
        | (Some aS,Some bS) ->
           Some (Function(aS,bS))
        | _ -> None
(*
    | PiAbstraction(_,t,b) ->
        let bodySig = getSignatureOfTerm b
        let argSig = getSignatureOfTerm t
        match (argSig,bodySig) with
        | (Some aS,Some bS) ->
           Some (Function(aS,bS))
        | _ -> None        
*)
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
    | PrimFn _ -> term
    | LitConstant _ -> term
    | Variable(AxiomVar(_,_)) -> term
    | Variable(TypedVar(n,t)) -> 
        if n = oldName then Variable(TypedVar(newName,t)) else term
    | LambdaAbstraction(n,t,b) ->
        let name = if n = oldName then newName else n
        LambdaAbstraction(name,t,(renameVariableInTerm b oldName newName))
    | Application(tA,tB) -> 
        Application((renameVariableInTerm tA oldName newName),
                    (renameVariableInTerm tB oldName newName))
(*
    | PiAbstraction(p,pt,pb) ->
        let name = if p = oldName then newName else p
        PiAbstraction(name,pt,(renameVariableInTerm pb oldName newName))
*)    
    

let rec alphaEquivalentTerms termA termB =
    match (termA,termB) with
    | (LitConstant a,LitConstant b) -> a = b
    | (PrimFn a, PrimFn b) -> a = b
    | (SortTerm (Sort.Simple stA), SortTerm (Sort.Simple stB)) -> stA = stB
    | (SortTerm (Sort.Inductive(stA,dA)), SortTerm (Sort.Inductive(stB,dB))) -> stA = stB && dA = dB
    | ((Variable (TypedVar(a,_))),Variable (TypedVar(b,_))) ->
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
    | ((PiAbstraction(_,tA,vA),PiAbstraction(_,tB,vB))) -> 
        let alphaEquivalentArguments = alphaEquivalentTerms tA tB
        if alphaEquivalentArguments then (alphaEquivalentTerms vA vB) else false
    | _ -> false

let rec getFreeVariablesOfTerm term =
    match term with
    | SortTerm _ -> []
    | Variable(TypedVar(n,t)) -> [TypedVar(n,t)]
    | Variable(AxiomVar(_,_)) -> []
    | LambdaAbstraction(n,t,b) ->
        let bodyVars = getFreeVariablesOfTerm b
        List.filter (fun x ->  not (x.Name = n)) bodyVars
    // Same logic as lambda
    | PiAbstraction(n,t,b) ->
        let bodyVars = getFreeVariablesOfTerm b
        List.filter (fun x ->  not (x.Name = n)) bodyVars        
    | Application(tA,tB) ->
        // not looking at types yet
        let varsA = getFreeVariablesOfTerm tA |> Set
        let varsB = getFreeVariablesOfTerm tB |> Set
        let intersection = Set.intersect varsA varsB
        Set.difference (Set.union varsA varsB) intersection
        |> Set.toList
    | LitConstant _ -> []
    | PrimFn f -> []
        // match f with
        // | Add IntType -> [
        //     TypedVar(MachineGenName("a",0),Constant IntType);
        //     TypedVar(MachineGenName("b",0),Constant IntType)
        //     ]
        // | Sub IntType -> [
        //     TypedVar(MachineGenName("a",0),Constant IntType);
        //     TypedVar(MachineGenName("b",0),Constant IntType)
        //     ]


let rec applyTerm termA termB : Term =
    match termA with
    | LambdaAbstraction(n,t,b) ->
        match b with
        // An odd case but if so this is just a constant.
        | SortTerm _ -> b
        // We ignore the type here - the typechecker will make sure 
        // TODO: Make this a Term option?
        | Variable(TypedVar(nam,_)) ->
            match nam = n with
            | true -> termB
            | false -> b
        | _ -> notImpl()

let rec betaEquivalentTerms termA termB =
    match (termA,termB) with
    | (SortTerm (Sort stA), SortTerm (Sort stB)) -> stA = stB
    | ((Variable (TypedVar(a,_))),Variable (TypedVar(b,_))) ->
        // Beta-equivalence is just about renaming
        // hence we can drop the type and check the names
        a = b


type Context = {
    pts : PureTypeSystem
    vars : TypedVar list
}

let isVariableInContext typedVar context =
    List.contains (context.vars) typedVar

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
            match Seq.contains s1 (context.pts.sorts) with
            | true ->
                if Seq.exists (fun (x1,_) -> s1 = x1) (context.pts.axioms) then
                    ErrorCtx (sprintf "The sort %s does not exist in the context." (s1.StringVal))
                    // TODO: Fill this in!
                    // Need to check s2 doens't exist in context sorts and s1:s2 not in context rulez
                    else ErrorCtx ("")

            | false ->
                // TODO: Fill this in! 
                ErrorCtx ("")
    | TypedVar(_,t) -> // don't care about the name
        match t with
        | PrimFn _ -> failwith "not done"
        | SortTerm s -> match Seq.contains s (context.pts.sorts) with
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