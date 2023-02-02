module Graphs

open SparseMatrix.SparseMatrix

type Vertex<'A> = 'A

type Edge<'A> = 'A * 'A

type Graph<'A, 'B when 'A: comparison and 'B: equality> =
    val Vertices: Set<Vertex<'A>>
    val Edges: Set<Edge<'A>>
    val AdjMtx: SparseMatrix<'B>

    new(vertices, edges, adjMtx) =
        { Vertices = vertices
          Edges = edges
          AdjMtx = adjMtx }
