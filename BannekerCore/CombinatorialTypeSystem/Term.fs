module CombinatorialTypeSystem.Term

open Name
open BasicTypes
open BuiltinPrimitives
open Nat

/// Terms in a CTS are defined recursively below.
/// Note that by abuse of notation we are calling lambda-preterms Terms.
/// Their alpha- and beta- equivalence is used to define the actual PTS terms
/// used in type-checking, etc.
type Term =
    /// 
    | EmptyTerm
    /// Many primitives, sorts, etc, have unit-term bodies.
    | UnitTerm
    /// LitConstant and PrimFn exist metatheoretically. 
    /// LitConstant are all backend (Scheme) values.
    | LitConstant of Literal
    /// PrimFn is things that would be executed directly by the (Scheme) backend.
    /// In that sense, all terms that "do math or effects" are just chains of PrimFn / Scheme terms.
    | PrimFn of PrimitiveFunction
    /// We have things like "let List : * -> *" which is interpreted _ _ : (Inductive "*" 1) 
    | SortTerm of Sort
    /// Constructors are also Terms.
    /// data Void = 
    ///     <=> DataDefinition(UserName "Void",SortTerm Type,(UserName "",Empty))
    /// TODO: Go into further detail about type vs. data in paper
    | DataDefinition of dataName:Name*typeTerm:Term*constructor:(Name*Term)
    /// Wrapper for TypedVar.
    /// A TypedVar might be a sort relation (Type : Kind),
    /// but it's more interesting when it's TypedVar(name,typeVal,bodyVal).
    | Variable of TypedVar
    // BoundNames are allowed Terms, but they can only pass type-checking if a term
    | BoundName of Name*typeTerm:Term
    | Application of Term*Term

and TypedVar = 
    /// E.g. let myValue : MyType = MyTypeNatConstructor (S Z)
    /// <=> TypedVar(UserName "myValue",[the 
    | TypedVar of Name*typ:Term*body:Term
    | AxiomVar of Sort*Sort
with
    member x.Name = 
        match x with
        | TypedVar(n,_,_) -> n
        | AxiomVar(s,_) -> sortToName s

let rec listTermToApplication (xs: Term list) : Term =
    match xs with
    | [] -> EmptyTerm
    | t :: [] -> t
    | t :: (tnext :: ts) -> Application(t,listTermToApplication (tnext :: ts))

/// CompleteTypeDefinitions are a bit tautological in a CTS context -
/// it is simply the union of all DataConstructors(name,term,(nameA,termA)) with 
/// the same nameA it is well-typed if they have the same termA.
/// But it is useful to separate this out into its own construction for the compiler. 
type CompleteTypeDefinition = | Constructors of List<Name*Term*(Name*Term)>
with
    /// Private validation that makes sure a CompleteTypeDefinition really is
    /// what it says it is. That is, for each element (name,typeTerm,(conName,conArgType)),
    /// the name and typeTerm are the same **F#** objects, while the conName and conArgType are
    /// **distinct** F# objects. If not, I screwed something up, not the user.
    /// A CompleteTypeDefinition is not a Term that could exist and is evaluated
    /// within the theory - this is a metatheoretical construct which we need to
    /// be reliably correct.
    private member x.IsWellFormed =
        match x with
        | Constructors [] -> true
        | Constructors (x :: []) -> true
        | Constructrs ((nx,ntx,(nconnx,nconargx)) :: ((ny,nty,(nconny,nconargy)) :: ys)) ->
            if nx = ny and (ntx = nty) then true
            else failwithf "CompleteTypeDefinition object was given inconsistent constructors: NameA - %A, NameB - %A; typeA - %A, typeB - %A" nx ny ntx nty

    /// Helper method for converting each constructor to a separate Term, for a list of Terms.
    member x.ToTermList =
        match x with 
        | Constructors xs ->
            match xs with
            | [] -> [EmptyTerm]
            | _ -> List.map (fun (n,t,(na,ta)) -> DataDefinition(n,t,(na,ta))) xs

    /// While a type definition is, primitively, a union of DataDefinitions,
    /// it is syntactically possible to encode this into an single Term
    /// - a TypedVar where the "head" is the type name / type type,
    /// and the "tail" is the Application-union of the list of constructors.
    /// This single term is added to the context (provided each DataDefinition
    /// is well-typed) when the type is defined.
    member x.ToSingleTermForType =
        match x with
        | Constructors xs ->
            match xs with
            | [] -> EmptyTerm
            | _ ->
                let consTerm = listTermToApplication(xs)
                let

let inline dataConstructorToBoundName (n:Name,t:Term,cons:Name*Term) : Term =
    BoundName((fst cons),Application((snd cons),BoundName(n,t)))

let rec isTypeDefinition : Term -> bool = fun t ->
    match t with
    | DataDefinition(_) -> true
    | Application(DataDefinition(_),tb) -> isTypeDefinition tb
    | _ -> false

let natName : Name = UserName "Nat"
let zeroName : Name = UserName "Z"
let succName : Name = UserName "S"

let ctsNatType = BoundName(natName, SortTerm Type)

/// Nats are a fundamental builtin to a CTS
let ctsNatTypedef : CompleteTypeDefinition =
    Constructors [
        (natName,SortTerm Type,(zeroName,UnitTerm));
        (natName,SortTerm Type,(succName,ctsNatType))
    ]

let rec fSharpNatToCTSNat (n:Nat) : Term =
    match n with
    | Z -> Application(BoundName(zeroName,ctsNatType),UnitTerm)
    | S x -> Application(BoundName(succName,Application(ctsNatType,ctsNatType)), fSharpNatToCTSNat x)

/// Interface for F# objects that can be expressed as Terms
type ITermable =
    abstract member ValueToTerm : unit -> Term
    abstract member TypeToTerm : Sort -> Term
    
let rec renameVariableInTerm term oldName newName =
    match term with
    | SortTerm _ -> term
    | PrimFn _ -> term
    | LitConstant _ -> term
    | Variable(AxiomVar(_,_)) -> term
    | Variable(TypedVar(n,t,b)) -> 
        if n = oldName then Variable(TypedVar(newName,t,b)) 
        else
            // Want to be able to rewrite into types and body values 
            let typeRewrite = renameVariableInTerm t oldName newName
            let bodyRewrite = renameVariableInTerm b oldName newName
            Variable(TypedVar(n,typeRewrite,bodyRewrite))
    | Application(tA,tB) -> 
        Application((renameVariableInTerm tA oldName newName),
                    (renameVariableInTerm tB oldName newName))        


let rec getFreeVariablesOfTerm term =
    match term with
    | SortTerm _ -> []
    | Variable(TypedVar(n,t,b)) -> [TypedVar(n,t,b)]
    | Variable(AxiomVar(_,_)) -> []
    | Application(tA,tB) ->
        // not looking at types yet
        let varsA = getFreeVariablesOfTerm tA |> Set
        let varsB = getFreeVariablesOfTerm tB |> Set
        let intersection = Set.intersect varsA varsB
        Set.difference (Set.union varsA varsB) intersection
        |> Set.toList
    | LitConstant _ -> []
    | PrimFn f -> []
                    
// let rec isDataStructureFinite (constructors:(DataConstructor list)) : bool =
//     match constructors with
//     | [] -> true
//     | [Unitary] -> true
//     | Unitary :: ys -> true
//     | [Function [TermArg(_,_)]] -> true
//     | [Function [Recursive]] -> false
//     | [Function xs] -> failwith "not done"
//     | Function xs :: ys -> failwith "not done"
