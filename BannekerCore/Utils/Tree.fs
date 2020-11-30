module Tree

open Utils
open BinaryRelation

type Tree<'TVertex> =
    | Leaf of 'TVertex
    | Branch of 'TVertex * (NonemptyList<Tree<'TVertex>>)

    member x.Head =
        match x with
        | Leaf v -> v
        | Branch(v,_) -> v

    member x.rest : Tree<'TVertex> list =
        match x with
        | Leaf _ -> []
        | Branch(_, ls) -> ls.ToList

    member x.VertexList() =
        let rec innerLoop cont t =
            match t with
            | Leaf v -> cont [v]
            | Branch(v,tl) ->
                match tl.rest with
                | [] -> innerLoop (fun acc -> cont (v :: acc)) tl.head
                | tree1 :: trees ->
                    innerLoop
                        (fun accumulator -> List.append accumulator (cont (tl.head.VertexList())))
                        (Branch(v,{head=tree1;rest=trees}))

        innerLoop id x

    member x.AdjacencyList() : ('TVertex * 'TVertex) list =
        let rec innerLoop cont t =
            match t with
            | Leaf _ -> cont []
            | Branch(v,tl) ->
                match tl.head,tl.rest with
                | (Leaf w, []) -> cont [(v,w)]
                | (Leaf w,tree :: trees) ->
                    innerLoop
                        (fun acc -> List.append (cont ((v,w) :: acc)) (tree.AdjacencyList()))
                        (Branch(v,{head=tree;rest=trees}))
                | (Branch(w,tl),[]) ->
                    innerLoop (fun acc -> cont ((v,w) :: acc)) (Branch(w,tl))
                | (Branch(w,tl),tree::trees) ->
                    notImpl()
        innerLoop id x

    interface IBinaryRelation<'TVertex> with
        member x.Domain() = x.VertexList()
        member x.AsPairList() = notImpl()


[<RequireQualifiedAccess>]
module Tree =

    /// Implements foldl on a Tree - from root to leaves, "left" to "right" (defining l/r by the list structure defining branches.)
    /// NOTE: Noncommutative or nonassociative fold functions may lead to unpredictable results.
    (*
    Comment on implementation:
    We actually rearrange the tree while folding to make this function easier to maintain -
    specifically, in a case of a Branch(vertex, listTrees), we append the listTrees end-to-end when doing the fold.
    *)
    let fold (zero: 'U) (folder: 'U -> 'T -> 'U) (t:Tree<'T>) : 'U =
        let rec inner cont tr =
            match tr with
            | Leaf v -> cont (folder zero v)
            | Branch(v,ls) ->
                match ls.rest with
                | [] -> inner cont (Leaf v) // @optimize just copy-paste the above, don't make this do a jump.
                | [x] -> inner (fun state -> cont (folder state v)) x
                | x :: y :: xs ->
                    match x with
                    | Leaf u ->
                        // if we're at the end of the leftmost branch, transform the tree by taking the next branches and
                        // "appending" them to the end. Then fold along that.
                        inner (fun state -> cont (folder state v)) (Branch(u,{head=y;rest=xs}))
                    | Branch(u,ys) ->
                        // otherwise append to the ys and fold along that.
                        inner (fun state -> cont (folder state v)) (Branch(u, NonemptyList.append ls ys))
        inner id t
