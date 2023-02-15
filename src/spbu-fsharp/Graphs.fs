module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers

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
        Set.ofSeq (
            seq {
                for i = 1 to toIntConv mtx.Rows do
                    for j = 1 to toIntConv mtx.Columns do
                        let value = mtx[uint i, uint j]

                        if value <> Option.None then
                            yield (Set.ofList [ uint i - 1u; uint j - 1u ])
            }
        )

    member this.Vertices = verticesOfMtx adjMtx

    member this.Edges = edgesOfMtx adjMtx

    member this.AdjMtx = adjMtx
