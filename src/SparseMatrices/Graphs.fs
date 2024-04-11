module SparseMatrices.Graphs

open SparseMatrices.SparseMatrix
open SparseMatrices.Helpers

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

    member this.Vertices = verticesOfMtx adjMtx
    member this.VerticesCount = this.Vertices.Count

    member this.Edges = edgesOfMtx adjMtx
    member this.EdgesCount = this.Edges.Count

    member this.Density =
        (float this.EdgesCount) / float (this.Vertices.Count * this.Vertices.Count)

    member this.AdjMtx = adjMtx
