module Graphs

open SparseMatrix.SparseMatrix

type Vertex<'A> = 'A

type Edge<'A when 'A: comparison> = Map<Vertex<'A>, Vertex<'A>>

type Graph<'A, 'B when 'A: comparison and 'B: equality> =
    val vertices: Set<Vertex<'A>>
    val edges: Set<Edge<'A>>
    val adjMtx: SparseMatrix<'B>
