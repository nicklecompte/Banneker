module CombinatorialTypeSystem.BasicTypes

open Name
open Nat

type Sort = 
    | Simple of string
    // For Calculus of Inductive Constructions, etc
    // This automatically defines an axiom in a PTS:
    // Given s = InductiveFamily(t), then (s,t) \in \mathcal{R}
    | Inductive of root:string*depth:Nat
with
    member inline x.StringVal = 
        match x with 
            | Simple s -> s
            | Inductive(s,i) -> sprintf "%s_%i" s (Nat.toInt i)
       //     | InductiveFamily f

type PTSAxiom =
    | Simple of Sort*Sort
    | Inductive of root:string*pair:(Nat*Nat)

type PTSRule =
    | Simple of Sort*Sort
    | Inductive of root: string * map: (Nat -> Nat -> Nat)

type PureTypeSystem = {
    sorts : Sort list 
    axioms : PTSAxiom list
    rules : PTSRule List
}

type ValidatedPTS =
    | ValidPTS of PureTypeSystem
    | InvalidPTS of string