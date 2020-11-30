module Utils

/// Optics, based largely on Aether: https://github.com/xyncro/aether/
[<AutoOpen>]
module Lenses =
    /// A Lens is a pair of a getter and a setter. Typically the 'a is an "outer" type and the 'b is an "inner" type.
    /// Getter: takes an 'a and returns its inner 'b.
    /// Setter: takes a 'b and an outer 'a, and returns an 'a with it's 'b value equal to the supplied 'b.
    /// Example: we have a record type Employee = {id=Guid,name=string}. We want a Lens<Employee,string>.
    /// Getter: fun emp -> emp.name
    /// Setter: fun newName emp = {emp with name = newName.}
    type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

    /// Compose two lenses. 
    /// If we have a lens from 'a to its inner 'b, and a lens from 'b to its inner 'c,
    /// compose returns the lens from 'a to its (inner) inner 'c.
    let composeLens ((g1,s1):Lens<'a,'b>) ((g2,s2):Lens<'b,'c>) : Lens<'a,'c> =
            (fun a -> g2 (g1 a)), (fun c a -> s1 (s2 c (g1 a)) a)
    
    let setWithLens (a:'a) (b:'b) ((_,setter): Lens<'a,'b>) =
        setter b a

    let getWithLens (a:'a) ((getter,_): Lens<'a,'b>) =
        getter a

    let mapWithLens ((getter,setter): Lens<'a,'b>) (f: 'b -> 'b) (a:'a) =
        setter (f (getter a)) a

type OKResult<'T> =
    | OK
    | Error of 'T

let notImpl() = failwith "not done"

type NonemptyList<'T> = {
    head : 'T
    rest : 'T list
}
with
    member x.ToList = x.head :: x.rest

    member x.Tail : NonemptyList<'T> option =
        match x.rest with
        | [] -> None
        | l :: ls -> Some ({head = l; rest = ls})

[<RequireQualifiedAccess>]
module NonemptyList =
    let head x = x.head

    let tail (x:NonemptyList<'T>) = x.Tail

    let append xs ys =
        match xs.rest with
        | [] -> {head=xs.head;rest=ys.head::ys.rest}
        | _ -> {head=xs.head;rest=List.append xs.rest (ys.head::ys.rest)}

    let map mapping nlist =
        {head = (mapping nlist.head);rest = (List.map mapping nlist.rest)}


// type Tree<'T> =
//     | Empty
//     | Leaf of 'T
//     | Node of Tree<'T> list

type InformativeEquality<'T> =
    | Equal of a:'T * b:'T
    | Unequal of a:'T*b:'T*infoMessage:string

type KeyedTree<'K,'T when 'K : equality> =
    | Empty
    | Leaf of 'K * 'T
    | Node of ('K * 'T) * (KeyedTree<'K,'T> list)

module KeyedTree =
    let rec root tree =
        match tree with
        | Empty -> ValueNone
        | Leaf(k,t) -> ValueSome (k,t)
        | Node ((k,t),_) -> ValueSome (k,t)

    let rec get tree key =
        match tree with
        | Empty -> ValueNone
        | Leaf(k,t) -> 
            match k = key with
            | true -> ValueSome t
            | false -> ValueNone
        | Node((k,t),[]) ->
            get (Leaf(k,t)) key
        | Node((k,t), tr::ts) ->
            match k = key with
            | true -> ValueSome t
            | false -> 
                match get tr key with
                | ValueSome t -> ValueSome t
                | ValueNone ->
                    get (Node((k,t),ts)) key 