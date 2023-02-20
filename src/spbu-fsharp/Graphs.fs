module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers

type Graph<'A when 'A: equality>(adjMtx: SparseMatrix<'A>) =

    let verticesOfMtx (mtx: SparseMatrix<'A>) =
        Set.ofSeq (
            seq {
                for i = 1 to (toIntConv mtx.Rows) do
                    yield uint i
            }
        )

    let edgesOfMtx (mtx: SparseMatrix<'A>) =

        let folder i j _ set = Set.add (Set.ofList [ i; j ]) set
        SparseMatrix.Fold folder Set.empty mtx

    member this.Vertices = verticesOfMtx adjMtx

    member this.Edges = edgesOfMtx adjMtx

    member this.AdjMtx = adjMtx
