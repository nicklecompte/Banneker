module Utils

type OKResult<'T> =
    | OK
    | Error of 'T

let notImpl() = failwith "not done"

// type Tree<'T> =
//     | Empty
//     | Leaf of 'T
//     | Node of Tree<'T> list

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
        | Node((k,t), ts) ->
            match k = key with
            | true -> ValueSome t
            | false -> 