module CombinatorialTypeSystem.Nat

type Nat =
    | Z
    | S of Nat
with
    static member Zero = Z
    static member One = (S Z)
    static member (+) (a,b) =
        match (a,b) with
        | (Z,Z) -> Z
        | (Z,_) -> b
        | (_,Z) -> a
        | (S x,_) -> S (x + b)
    static member (*) (a,b) =
        match (a,b) with
        | (Z,_) -> Z
        | (_,Z) -> Z
        | (S x, _) -> b + (x * b)
    // Built-in >, <, >=, <= works for this.
    // interface ITermable with
    //     member _.TypeToTerm(s:Sort) =
    //         Variable (TypedVar(UserName("Nat"),SortTerm s,Empty))
    //     member x.ValueToTerm() =
    //         failwith "not done"
    //         // match x with
    //         // | Z -> Variable (TypedVar(UserName "Z"),SortTerm )

type Fin(valu:Nat,bound:Nat) =
    do
        if valu > bound
            then failwithf "Fin was given %A as the value and %A as the bound" valu bound
    member __.Value = valu
    member __.Bound = bound 
    static member Zero = Z
    static member One = S Z
    static member (+) (a:Fin,b:Fin) =
        if a.Bound <> b.Bound
            then failwithf "tried to add %A and %A with unmatched bounds %A and %A" a b (a.Bound) (b.Bound)
        let intermediate = (a.Value) + (b.Value)
        if intermediate > a.Bound
            then failwithf "tried to add %A and %A which exceeded the bound %A" a b (a.Bound)
        Fin(intermediate,a.Bound)     

type PrimitiveRecursiveFunction ={
    baseCase : Nat
    inductiveFunc: Nat -> Nat
}


[<RequireQualifiedAccess>]
module Nat =
    let isZero n = 
        match n with 
        | Z -> true 
        | _ -> false

    let rec fromInt z =
        match z > 0 with
        | true -> S (fromInt (z - 1))
        | false -> Z

    let rec toInt n =
        match n with
        | Z -> 0
        | S x -> 1 + (toInt x)