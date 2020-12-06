/// This module defines the inductiveView function which 
///
module CombinatorialTypeSystem.InductiveView

open Name
open Term
open BasicTypes
open Nat

/// A simple "interface" for a context.
type Context = {
    getTermByName : Name -> Term option
    getDefinitionFromTypeName : Name -> CompleteTypeDefinition option
}

type InductiveView =
    | BaseCase
    | SelfRecursive of Nat
    | OtherRecursive of (Name*Term)*InductiveView
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

let bindApplicationTermIVRsult (a : Term) (b : Term) (m: Term -> InductiveViewResult) : InductiveViewResult =
    match m a with
    | IV ivA ->
        match m b with
        | IV ivB -> IV (Sequential(ivA,ivB))
        | ivresult -> ivresult
    | ivresult -> ivresult

let bindSequentialIVResult (a: InductiveViewResult) (b: InductiveViewResult) : InductiveViewResult =
    match (a,b) with
    | (IV ivA,IV ivB) -> IV (Sequential(ivA,ivB))
    | (x,IV ivB) -> x
    | (IV ivA,x) -> x

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
    (termEquality : Term*Term -> bool)
    // Function that determines if two terms have the same type
    (sameType : Term*Term -> bool) : InductiveViewResult = 
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
        | DataDefinition(nm,ty,(_,ctype)) -> 
            match ctype with
             // data MyType = | SingletonCase
            | UnitTerm -> IV BaseCase
            //  data MyType:SomeSort = | Somecase of SomeType:SomeOtherSort
            | BoundName(na,nt) ->
                // data MyType : SomeSort = | Rec of MyType:SomeSort
                if nm = na && (termEquality(nt,ctype)) then IV (SelfRecursive Z)
                // data Mytype : SomeSort = | Inner of OtherType:OtherSort (in theory at this point)
                // First have to check that the name na actually exists in the context,
                // and if so get the DataDefinition / Application(DataDefinijon ...) corresponding
                // to the type nm (assuming it exists)
                //else bindNameValue na context inner
            | Application(BoundName(na,nta), BoundName(nb,ntb)) ->
                match (context na, context nb) with
                | (Some ta,Some tb) -> inner (Application(ta, tb))
                | (_, None) -> NameNotBoundInContext na
                | (None, _) -> NameNotBoundInContext nb
            | Application(BoundName(na,nta), Application(tx,ty)) ->
                bindSequentialIVResult
                    (bindNameValue na context inner)
                    (inner (Application(tx,ty)))
            | Application(Application(tx,ty),BoundName(na,nta)) ->               
                bindSequentialIVResult
                    (inner (Application(tx,ty)))
                    (bindNameValue na context inner)
            // TODO: Is this it? Should we investigate the possibility of "extraneous"
            // TODO: units? Here? In higher-level language? At all?
            | _ -> TermNotWellFormed t
        // let x : t = b
        | Variable(TypedVar(nm,ty,b)) ->
            match b with
            // something like prim__str_len(mystr) which will always return a PrimFn or LitConstant
            | Application(PrimFn _,LitConstant _) -> IV BaseCase
            // nm : ty = funname:funntype body with funname:funntype in the context
            | Application(BoundName(funname,funntype),body) -> 
                match (context funname) with
                // data na : ta = | conname of conbody
                // Since x : myType = n(body) => the conname must == n and body == conbody
                // data n : ta = conname of conbody
                | Some (DataDefinition(typenam,typetype,(conname,conbody))) ->
                    if conname = funname then
                        // 
                        if (termEquality (ty,typenam)) then
                            // the body and conbody have to have matching types
                            // TODO: Where exactly 
                            if (sameType(body,conbody)) then
                            else NonmatchingType body
                        else TermNotWellFormed t
                    else TermNotWellFormed t
                | Some (Application(t1,t2)) -> failwithf "not done"
                // TODO: Is this it for well-formed options?
                | Some _ -> TermNotWellFormed t
                | None -> NameNotBoundInContext na
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
let inductiveViewTypeCTS : Term = BoundName(inductiveViewName, SortTerm Type)  

let inductiveBaseCaseConstructor = 
    (inductiveViewName , SortTerm Type,(UserName "BaseCase",UnitTerm))

let inductiveSelfRecursiveCaseConstructor = 
    (inductiveViewName , SortTerm Type,(UserName "SelfRecursive",BoundName(natName,SortTerm Type)))

let inductiveOtherRecursiveCaseConstructor =
    (inductiveViewName , SortTerm Type,(UserName "OtherRecursive",Application(ctsNatType,inductiveViewTypeCTS)))

let inductiveSequentialCaseConstructor =
    (inductiveViewName , SortTerm Type,(UserName "Sequential",Application(inductiveViewTypeCTS,inductiveViewTypeCTS)))

// dataName:Name*typeTerm:Term*constructor:(Name*Term)
let inductiveViewTypedefCTS : CompleteTypeDefinition = 
    Constructors [
        inductiveBaseCaseConstructor;
        inductiveSelfRecursiveCaseConstructor;
        inductiveOtherRecursiveCaseConstructor;
        inductiveSequentialCaseConstructor
    ]  

let fSharpinductiveViewToCTS (inductView:InductiveView) : Term =
    let rec inner inView =
        match inView with
        | BaseCase -> 
            Application(
                dataConstructorToBoundName inductiveBaseCaseConstructor,
                UnitTerm)
        | SelfRecursive n -> 
            Application(
                BoundName(UserName "SelfRecursive",Application(ctsNatType,inductiveViewTypeCTS)),
                fSharpNatToCTSNat n)
        | OtherRecursive(nam,i) -> 
            Application(
                BoundName(UserName "OtherRecursive",inductiveViewTypeCTS),
                Application(BoundName nam, inner i))
        | Sequential(iA,iB) -> 
            Application(
                    BoundName(UserName "Sequential",inductiveViewTypeCTS),
                    Application(inner iA, inner iB)
                 )
    inner inductView
