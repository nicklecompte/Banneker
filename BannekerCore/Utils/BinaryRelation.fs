module BinaryRelation

open Utils

type IBinaryRelation<'TVertex> =
    abstract member Domain : unit -> 'TVertex list
    abstract member AsPairList: unit -> ('TVertex* 'TVertex) list


type BinaryRelation<'T> = {
    domain : seq<'T>
    relations: 'T -> 'T list
}
with
    interface IBinaryRelation<'T> with
        member x.Domain() = x.domain |> Seq.toList
        member x.AsPairList() =
            x.domain
//                |> Seq.map(fun a -> (a,x.relations a))
            |> Seq.toList
            |> List.collect(fun a -> x.relations a |> List.map(fun b -> (a,b)))

type AdjacencyMatrix(relation:BinaryRelation<'T>) =
    member x.Name = notImpl()

[<AutoOpen>]
module BinaryRelation =

    let adjacencyPreservingMap mapping relation = notImpl()