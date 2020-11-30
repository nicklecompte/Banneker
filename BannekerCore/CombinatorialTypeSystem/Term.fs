module CombinatorialTypeSystem.Term

open Name
open BasicTypes
open BuiltinPrimitives
open Nat

/// Terms in a PTS are defined recursively below.
/// Note that by abuse of notation we are calling lambda-preterms Terms.
/// Their alpha- and beta- equivalence is used to define the actual PTS terms
/// used in type-checking, etc.
type Term =
    /// Many primitives, sorts, etc, have "empty" bodies.
    | Empty
    /// Unit term
    | Unit 
    /// LitConstant and PrimFn exist metatheoretically. 
    /// LitConstant are all backend (Scheme) values.
    | LitConstant of Literal
    /// PrimFn is things that would be executed directly by the (Scheme) backend.
    /// In that sense, all terms that "do math or effects" are just chains of PrimFn / Scheme terms.
    | PrimFn of PrimitiveFunction
    /// We have things like "let List : * -> *" which is interpreted _ _ : (Inductive "*" 1) 
    | SortTerm of Sort
    /// Constructors are also Terms.
    /// TODO: Go into further detail about type vs. data in paper
    /// TODO: Named arguments in this? Maybe just a frontend issue?
    | DataDefinition of typeTerm:Term*constructors:((Name*Term) list)
    /// Wrapper for TypedVar.
    /// A TypedVar might be a sort relation (Type : Kind),
    /// but it's more interesting when it's TypedVar(name,typeVal,bodyVal).
    | Variable of TypedVar
    | PiAbstraction of argumentName:Name*argumentType:Term * varBody : Term
    | Application of Term*Term

and TypedVar = 
    /// E.g. let myValue : 
    | TypedVar of Name*typ:Term*body:Term
    | AxiomVar of Sort*Sort
with
    member x.Name = 
        match x with
        | TypedVar(n,_,_) -> n
        | AxiomVar(s,_) -> sortToName s


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
    | PiAbstraction(n,t,b) ->
        let name = if n = oldName then newName else n
        let typeRewrite = renameVariableInTerm t oldName newName
        let bodyRewrite = renameVariableInTerm b oldName newName
        PiAbstraction(name,typeRewrite,bodyRewrite)
    | Application(tA,tB) -> 
        Application((renameVariableInTerm tA oldName newName),
                    (renameVariableInTerm tB oldName newName))        


let rec getFreeVariablesOfTerm term =
    match term with
    | SortTerm _ -> []
    | Variable(TypedVar(n,t,b)) -> [TypedVar(n,t,b)]
    | Variable(AxiomVar(_,_)) -> []
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
                    
// let rec isDataStructureFinite (constructors:(DataConstructor list)) : bool =
//     match constructors with
//     | [] -> true
//     | [Unitary] -> true
//     | Unitary :: ys -> true
//     | [Function [TermArg(_,_)]] -> true
//     | [Function [Recursive]] -> false
//     | [Function xs] -> failwith "not done"
//     | Function xs :: ys -> failwith "not done"