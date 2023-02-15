module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers
open Trees.QuadTrees

type Vertex<'A> = 'A

type UndirectedEdge<'PairOfVertices when 'PairOfVertices: comparison> = Set<'PairOfVertices>

type Graph<'A when 'A: equality>(adjMtx: SparseMatrix<'A>) =

    let verticesOfMtx (mtx: SparseMatrix<'A>) : Set<Vertex<uint>> =
        Set.ofSeq (
            seq {
                for i = 1 to (toIntConv mtx.Rows) do
                    yield uint i
            }
        )

    let edgesOfMtx (mtx: SparseMatrix<'A>) : Set<UndirectedEdge<uint>> =
        let mutable edges = Set.empty

        let rec inner (tree, i: uint, j: uint, size: uint) =
            match tree with
            | QuadTree.None -> ()
            | QuadTree.Leaf _ -> edges <- edges.Add(Set.ofList [ i; j ])
            | QuadTree.Node(nw, ne, sw, se) ->
                let half = size / 2u
                inner (nw, i, j, half)
                inner (ne, i, j + half, half)
                inner (sw, i + half, j, half)
                inner (se, i + half, j + half, half)

        inner (mtx.Data, 0u, 0u, ceilPowTwo (max mtx.Rows mtx.Columns))
        edges

    member this.Vertices = verticesOfMtx adjMtx

    member this.Edges = edgesOfMtx adjMtx

    member this.AdjMtx = adjMtx
