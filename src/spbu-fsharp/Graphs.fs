module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers
open Trees.QuadTrees

type Graph<'A when 'A: equality>(adjMtx: SparseMatrix<'A>) =

    let verticesOfMtx (mtx: SparseMatrix<'A>) =
        Set.ofSeq (
            seq {
                for i = 1 to (toIntConv mtx.Rows) do
                    yield uint i
            }
        )

    let edgesOfMtx (mtx: SparseMatrix<'A>) =

        let rec inner collector tree result i j size =
            match tree with
            | QuadTree.None -> result
            | QuadTree.Leaf _ -> collector i j result
            | QuadTree.Node(nw, ne, sw, se) ->
                let half = size / 2u
                let resultFromNW = inner collector nw result i j half
                let resultFromNE = inner collector ne resultFromNW i (j + half) half
                let resultFromSW = inner collector sw resultFromNE (i + half) j half
                inner collector se resultFromSW (i + half) (j + half) half

        let collector i j set = Set.add (Set.ofList [ i; j ]) set
        let size = ceilPowTwo (max mtx.Rows mtx.Columns)
        inner collector mtx.Data Set.empty 0u 0u size

    member this.Vertices = verticesOfMtx adjMtx

    member this.Edges = edgesOfMtx adjMtx

    member this.AdjMtx = adjMtx
