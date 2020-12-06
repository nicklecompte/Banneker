module Graph

open Utils
open BinaryRelation
open Tree

/// Taken from Alga: https://github.com/snowleopard/alga
/// Generic inductively-defined directed graph structure. We consider a Graph<'TVertex> as being represented by
/// the pair {E,V} where E \subset 'TVertex and V \subset 'TVertex*'TVertex
(*
Some comments:
    - Note that hypergraphs are NOT supported, but loops are supported.
    - Although it would be more proper to create undirected graphs by Overlay( (Connected(a,b)), (Connected(b,a)),
        it is probably more sensible for the caller to "assume" and simply check (v1,v2) or (v2,v1) for adjacency, returning true when the first one hits.
*)
type Graph<'TVertex when 'TVertex : equality> =
    | EmptyGraph
    /// The graph consisting of a single vertex from 'T (edgeset is the empty set)
    | Singleton of 'TVertex
    /// The graph built from two graphs G1 = {E1,V1} and G2 = {E2,V2} by G_overlay = {V1 U V2, E1 U E2}
    | Overlay of Graph<'TVertex> * Graph<'TVertex>
    /// The graph built from two graphs G1 = {E1,V1} and G2 = {E2,V2} by G_connect = {V1 U V2, E1 U E2 U (V1 × V2)}
    | Connected of Graph<'TVertex> * Graph<'TVertex>

    member x.ToVertexSet() : 'TVertex list =
        match x with
        | EmptyGraph -> []
        | Singleton x -> [x]
        // TODO: Not very efficient.
        | Overlay (x,y) -> (x.ToVertexSet()) |> List.append (y.ToVertexSet()) |> List.distinct
        // TODO: Not very efficient.
        | Connected (x,y) -> (x.ToVertexSet()) |> List.append (y.ToVertexSet()) |> List.distinct

    member x.ToEdgeSet() =
        match x with
        | EmptyGraph -> []
        | Singleton _ -> []
        | Overlay (x,y) ->
            x.ToEdgeSet()
            |> List.append (y.ToEdgeSet())
        | Connected (g1,g2) ->
            g1.ToEdgeSet()
            |> List.append (g2.ToEdgeSet())
            |> List.append (g1.ToVertexSet() |> List.zip (g2.ToVertexSet()))

    override x.ToString() =
        match x with
        | EmptyGraph -> "empty graph"
        | Singleton x -> x.ToString()
        | Overlay (a,b) -> notImpl()
        | Connected (a,b) -> notImpl()

    interface IBinaryRelation<'TVertex> with
        member x.Domain() = x.ToVertexSet()
        member x.AsPairList() = x.ToEdgeSet()

/// An optional annotation to a Graph<'TVertex>. Primarily used for choosing optimal algorithms.
type SpecificGraphType<'TVertex> =
    | Trivial
    | Complete
    | Star of center:'TVertex
    | Tree
    | Unspecified

/// An edge-labelled graph is a pair of a Graph<'T> and a function 'T*'T -> 'TLabel voption.
type EdgeLabelledGraph<'TVertex, 'TEdgeLabelling when 'TVertex : equality> = {
    graph: Graph<'TVertex>
    labelling: 'TVertex*'TVertex -> voption<'TEdgeLabelling>
}
with
    member x.ToVertexSet() = x.graph.ToVertexSet()

    member x.ToUnlabelledEdgeSet() = x.graph.ToEdgeSet()

    member x.ToLabelledEdgeSet() = x.ToUnlabelledEdgeSet() |> List.map(fun a -> (a, x.labelling a))

    static member GraphLens : Lens<EdgeLabelledGraph<'TVertex,_>,Graph<'TVertex>> =
        ((fun a -> a.graph), (fun g a -> {a with graph = g}))

/// Descriptive exception-like objects for operations on Graphs.
type GraphOperationException<'TVertex when 'TVertex : equality> =
    | InvalidVertex of 'TVertex
    | InvalidGraph of Graph<'TVertex>



[<RequireQualifiedAccess>]
module Graph =
    open System.Collections.Generic

    /// Applies a fold to a Graph<'T>, given a mapping 'T -> 'U and a zero value in 'U.
    /// Here 'U is given the structure of something like a ring.
    /// There are two binary operators on 'U which are supplied as arguments to fold.
    /// Possibly overlayFunc can be considered "additive" and connectedFunc "multiplicative"
    /// but there's no structure enforcing anything like that.
    /// <param name="zero"> Some zero value in 'U considered as a ring (ish). </param>
    /// <param name="map"> A function 'T -> 'U. </param>
    /// <param name="overlayFunc"> A binary operation 'U -> 'U -> 'U. </param>
    /// <param name="connectedFunc"> A binary operation 'U -> 'U -> 'U. </param>
    /// <param name="graph"> A Graph<'T>. </param>
    /// <returns> A value in 'U. </returns>
    let foldGraph (zero:'U) (map: 'T -> 'U) (overlayFunc: 'U -> 'U -> 'U) (connectedFunc : 'U -> 'U -> 'U) (g: Graph<'T>) : 'U =
        let rec inner cont graph =
            match graph with
            | EmptyGraph -> cont zero
            | Singleton t -> cont (map t)
            | Overlay(a,b) ->
                // In the continuation, pull in inner of a,
                // then apply overlayFunc to that when doing inner of b.
                inner (fun u -> overlayFunc (inner cont a) u) b
            | Connected(a,b) ->
                // In the continuation, pull in inner of a and b,
                // then apply connectedFunc to that when doing inner of b.
                inner (fun u -> connectedFunc (inner cont a) u) b
        inner id g


    /// Takes in a Graph<'T> and returns a Graph<'T> which has the same vertex and edge sets, but with any extraneous EmptyGraphs removed.
    /// For instance, simplifyGraph (Connected(Singleton("cat"),EmptyGraph)) = Singleton("cat").
    /// TODO: Improve rest-recursiveness with CPS.
    let rec removeEmptySubgraphs : Graph<'T> -> Graph<'T> = fun g ->
        match g with
        | EmptyGraph -> g
        | Singleton _ -> g
        | Overlay(EmptyGraph,h) -> removeEmptySubgraphs h
        | Overlay(h,EmptyGraph) -> removeEmptySubgraphs h
        | Overlay(h,i) ->
            let reducedH = (removeEmptySubgraphs h)
            let reducedI = (removeEmptySubgraphs i)
            match (reducedH,reducedI) with
            | (EmptyGraph,EmptyGraph) -> EmptyGraph
            | (EmptyGraph,_) -> reducedI
            | (_,EmptyGraph) -> reducedH
            | _ -> Overlay(reducedI, reducedH)
        | Connected(EmptyGraph,h) -> removeEmptySubgraphs h
        | Connected(h,EmptyGraph) -> removeEmptySubgraphs h
        | Connected(h,i) ->
            let reducedH = (removeEmptySubgraphs h)
            let reducedI = (removeEmptySubgraphs i)
            match (reducedH,reducedI) with
            | (EmptyGraph,EmptyGraph) -> EmptyGraph
            | (EmptyGraph,_) -> reducedI
            | (_,EmptyGraph) -> reducedH
            | _ -> Connected(reducedI, reducedH)

    let contains vertex graph : bool =
        let rec inner g cont =
            match g with
            | EmptyGraph -> cont EmptyGraph
            | Singleton x -> x = vertex || cont g
            | Connected(a, b) ->
                inner b (fun g -> inner a cont || cont g)
            | Overlay(a, b) ->
                inner b (fun g -> inner a cont || cont g)
        inner graph (fun _ -> false)

    let removeVertex vertex graph : Result<Graph<'T>,GraphOperationException<'T>> =
        match graph with
        | EmptyGraph -> Result.Error (InvalidGraph graph)
        | Singleton v ->
            match v = vertex with
            | true -> Ok EmptyGraph
            | false -> Result.Error (InvalidVertex vertex)


    /// Create a graph from any IBinaryRelation<'U>.
    let fromBinaryRelation<'U when 'U : equality > (rel: IBinaryRelation<'U>) =
        let vertices = rel.Domain()
        match vertices with
        | [] -> EmptyGraph
        | [x] ->
            let pairs = rel.AsPairList()
            match pairs with
            | [] -> Singleton x
            | [(a,b)] ->
                if a = x && a = b then Connected(Singleton(x),Singleton(x))
                else (invalidArg "rel" (sprintf "Binary relation supplied to Graph.fromBinaryRelation is invalid. Vertex set was [%A], yet the edgeset contained the edge (%A,%A)" x a b))
            | l -> (invalidArg "rel" (sprintf "Binary relation supplied to Graph.fromBinaryRelation is invalid. Vertex set was [%A], yet the edgeset contained the edgeset %A" x l))
        | _ ->
            let pairs = rel.AsPairList()
            let rec graphBuilder cont (foundVertices : HashSet<'U>) ls =
                match ls with
                | [] -> cont EmptyGraph
                | [v] -> notImpl()
                | _ -> notImpl()
            notImpl()

    /// Gets the Graph<'T> underlying a Tree<'T>.
    let rec fromTree (t: Tree<'T>) : Graph<'T> =
        let rec joinListAsOverlay (v:'T) (l: Tree<'T> list) : Graph<'T> =
            match l with
            | [] -> Singleton v
            | t::ts ->  notImpl()
        match t with
        | Leaf v -> Singleton v
        | Branch(v,ls) ->
            match ls.rest with
            | [] -> Singleton v
            | [t] -> Connected(Singleton v, fromTree t)
            | t :: l :: lss -> notImpl() // Overlay(Connected(Singleton v, fromTree t),)

        //match pairs with
        //| [] -> EmptyGraph
        //| [(x,y)] -> Connected ((Singleton x),(Singleton y))

    /// Create a "star" Graph<'T>, with one central vertex and some "spoke" vertices connected to it.
    /// root - the 'T at the center.
    /// outerVertices - a 'T list of outer vertices which will be joined to root.
    /// O(n) complexity.
    let createStar root outerVertices : Graph<'T> =
        // Recursive helper function for building the graph of outerVertices - distinct cliques of Singletons.
        // At the end we Connect this to the Singleton(root).
        // cont - continuation function : Graph<'T> -> Graph<'T>. Typically appends the argument graph to another graph supplied via closure.
        let rec inner cont l =
            match l with
            | [] -> cont EmptyGraph
            | [x] -> cont (Singleton(x))
            | x :: y :: xs ->
                inner (fun g ->
                                match g with
                                // Somewhat verbose short-circuiting so we don't have a dangling Overlay(EmptyGraph,_).
                                | EmptyGraph -> cont (Overlay(Singleton(x), Singleton(y)))
                                | _ -> cont (Overlay(g, Overlay(Singleton(x), Singleton(y))))
                      )
                      xs
        match outerVertices with
        | [] -> Singleton(root)
        | _ ->  let outer = (inner id outerVertices)
                Connected(Singleton(root), outer)


    let createCompleteGraph vertices =
        let rec inner cont ls =
            match ls with
            | [] -> cont EmptyGraph
            | x :: xs -> inner (fun g ->
                                        match g with
                                        // Somewhat verbose short-circuiting so we don't have a dangling Connected(EmptyGraph,_).
                                        | EmptyGraph -> cont (Singleton x)
                                        | _ -> cont (Connected(g,Singleton x)))
                                    xs
        // Small bit of preoptimization to short-circuit the inner subfunction for trivial graphs.
        match vertices with
        | [] -> EmptyGraph
        | [x] -> Singleton x
        | _ -> inner id vertices

    let determineGraphTypeFromGraph : Graph<'T> -> SpecificGraphType<'T> = fun g ->
        match g with
        | EmptyGraph -> Trivial
        | Singleton _ -> Trivial
        | _ -> notImpl()


//    let generateLabelledGraphFrom