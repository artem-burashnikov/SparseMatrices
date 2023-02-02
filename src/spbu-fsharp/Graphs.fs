module Graphs

open SparseMatrix.SparseMatrix
open Helpers.Numbers

type Vertex<'A> = 'A

type Edge<'A> = 'A * 'A


type Graph<'A when 'A: equality>(adjMtx: SparseMatrix<'A>) =

    let verticesOfMtx (mtx: SparseMatrix<'A>) : Set<Vertex<uint>> =
        Set.ofSeq (
            seq {
                for i = 1 to (toIntConv mtx.Rows) do
                    yield uint i
            }
        )

    let edgesOfMtx (mtx: SparseMatrix<'A>) : Set<Edge<uint>> =
        Set.ofSeq (
            seq {
                for i = 1 to toIntConv mtx.Rows do
                    for j = 1 to toIntConv mtx.Columns do
                        let value = mtx[uint i, uint j]

                        if value <> Option.None then
                            yield (uint i - 1u, uint j - 1u)
            }
        )

    let vertices = verticesOfMtx adjMtx

    let edges = edgesOfMtx adjMtx

    member this.Vertices = vertices

    member this.Edges = edges

    member this.AdjMtx = adjMtx
