module CombinatorialTypeSystem.InductiveView

open Name
open Term
open BasicTypes
open Nat

type InductiveView =
    | BaseCase
    | SelfRecursive of Nat
    | OtherRecursive of Name*InductiveView
    | Sequential of InductiveView * InductiveView


type InductiveViewResult =
    | IV of InductiveView
    | TermNotWellFormed of Term
    | ContextNotWellFormed
    | NonmatchingType of Term
    | NoMatchingDataDefinitionInContext of dataDefinition:Name*Term*(Name*Term)
    | NameNotBoundInContext of Name

let bindNameValue (n: Name) (context: Name -> Term option) (ivComp : Term -> InductiveViewResult) : InductiveViewResult =
    match context n with
    | Some term -> ivComp term
    | None -> NameNotBoundInContext n

/// inductiveView is a fundamental built-in that takes in any well-typed term,
/// and return
let computeInductiveView 
    (term:Term) 
    // Interface for a context
    (context:Name -> Term option) 
    // Function that evaluates Application(t1,t2)
    (evaluator : Term*Term -> Term option) 
    // Function that determines equality of terms
    // it seems like we just want plain (F# built-in) equality for everyting except bound names
    (termEquality : Term*Term -> bool): InductiveViewResult = 
    let rec inner t =
        match t with
        // TODO: Currently this is returning BaseCase,
        // TODO: for cases such as data Void = empty.
        // TODO: That is, Empty is well-typed but not constructible.
        // TODO: This *seeeeems* fine...
        // Empty is not a constructible well-typed term
        // and does not match to an inductive view
        | EmptyTerm -> IV BaseCase
        // inductiveView(1) = BaseCase
        // this is in the case (for instance) let x : 1 = 1, or if we have in
        // the context something like
        // data Mytype = | SimpleCase of 1
        // let x : Mytype = SimpleCase 1
        // TODO: Is this going to be confusing? Should we investigate Term = | Unit, | UnitCons
        // TODO: to distinguish "type identification" and "object"?
        | UnitTerm -> IV BaseCase
        // All the machine/VM primitives are BaseCase
        | LitConstant _ -> IV BaseCase
        // All the machine/VM primitives are BaseCase
        // TODO: Should the primitive functions be BaseCase or their arity?
        | PrimFn _ -> IV BaseCase
        // Type -> BaseCase, Kind n -> SelfRecursive(n + 1)
        | SortTerm s ->
            match s with
            | Type -> IV BaseCase
            | Kind n -> IV (SelfRecursive (n + (S Z)))
        | DataDefinition(n,t,(_,ctype)) -> 
            match ctype with
            | UnitTerm -> IV BaseCase
            | BoundName na -> 
                if n = na then IV (SelfRecursive (S Z))
                else bindNameValue na context inner
            | Application(BoundName na, BoundName nb) ->
                match (context na, context nb) with
                | (Some ta,Some tb) -> inner (Application(ta, tb))
                | (_, None) -> NameNotBoundInContext na
                | (None, _) -> NameNotBoundInContext nb
            | Application(BoundName na, Application(tx,ty)) ->
                // TODO: Fill in
                failwith "not done"
            | Application(Application(tx,ty),BoundName na) ->
                // TODO: Fill in
                failwith "not done"
            // TODO: Is this it? Should we investigate the possibility of "extraneous"
            // TODO: units? Here? In higher-level language? At all?
            | _ -> TermNotWellFormed t
        // let x : t = b
        | Variable(TypedVar(n,t,b)) ->
            match b with
            // something like prim__str_len(mystr) which will always return a PrimFn or LitConstant
            | Application(PrimFn _,LitConstant _) -> IV BaseCase
            // let x : myType = n(body) with n:t in the context
            | Application(BoundName n,body) -> 
                match (context n) with
                // data na : ta = | conname of conbody
                // Since x : myType = n(body) => the conname must == n and body == conbody
                // data n : ta = conname of conbody
                | Some (DataDefinition(na,ta,(conname,conbody))) ->
                    if conname = n then
                        if (termEquality (ta,t)) then failwithf "not done"
                        else TermNotWellFormed t
                    else TermNotWellFormed t
            | Application(t1,t2) -> 
                match evaluator (t1,t2) with
                | Some result -> inner result
                | None -> TermNotWellFormed t
        | Application(t1,t2) ->
            match isTypeDefinition t with
            | true -> 
                match inner t1 with
                | IV iv1 ->
                    match inner t2 with
                    | IV iv2 -> IV (Sequential(iv1,iv2))
                    | x -> x
                | x -> x
            | false -> 
                match evaluator(t1,t2) with
                | Some evalterm -> inner evalterm
                | None -> TermNotWellFormed t

    inner term


    
let inductiveViewName = UserName "inductiveView"
// dataName:Name*typeTerm:Term*constructor:(Name*Term)
let inductiveViewTypeCTS : CompleteTypeDefinition = 
    Constructors [
        (inductiveViewName , SortTerm Type,(UserName "BaseCase",UnitTerm));
        (inductiveViewName , SortTerm Type,(UserName "SelfRecursive",BoundName natName));
        (inductiveViewName , SortTerm Type,(UserName "OtherRecursive",Application(BoundName natName,BoundName inductiveViewName)));
        (inductiveViewName , SortTerm Type,(UserName "Seqeuntial",Application(BoundName inductiveViewName,BoundName inductiveViewName)))        
    ]

let fSharpinductiveViewToCTS (inductView:InductiveView) : Term =
    let rec inner inView =
        match inView with
        | BaseCase -> Application(BoundName(UserName "BaseCase"),UnitTerm)
        | SelfRecursive n -> Application(BoundName(UserName "SelfRecursive"),fSharpNatToCTSNat n)
        | OtherRecursive(nam,i) -> Application(BoundName(UserName "OtherRecursive"),Application(BoundName nam, inner i))
        | Sequential(iA,iB) -> Application(BoundName(UserName "Sequential"),Application(inner iA, inner iB))
    inner inductView