module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers

type Graph<'A when 'A: equality and 'A: comparison>(adjMtx: SparseMatrix<'A>) =

    let verticesOfMtx (mtx: SparseMatrix<'A>) =
        Set.ofSeq (
            seq {
                for i = 1 to (toIntConv mtx.Rows) do
                    yield uint i
            }
        )

    let edgesOfMtx (mtx: SparseMatrix<'A>) =

        let folder i j _ set = Set.add (i, j) set

        SparseMatrix.Fold folder Set.empty mtx

    let setOfVertices = verticesOfMtx adjMtx
    let setOfEdges = edgesOfMtx adjMtx

    member this.Vertices = setOfVertices
    member this.VerticesCount = this.Vertices.Count

    member this.Edges = setOfEdges
    member this.EdgesCount = this.Edges.Count

    member this.Density =
        (float this.EdgesCount) / float (this.Vertices.Count * this.Vertices.Count)

    member this.AdjMtx = adjMtx
