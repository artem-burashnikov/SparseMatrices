module SparseMatrices.BFS

open Microsoft.FSharp.Core

open SparseMatrices.MatrixAlgebra
open SparseMatrices.SparseVector
open SparseMatrices.Graphs

let fAdd a b =
    match a, b with
    | Some x, _
    | _, Some x -> Some x
    | _ -> Option.None

let fMult a b =
    match a, b with
    | Some x, Some _ -> Some x
    | _ -> Option.None

let fVisit a b =
    match a, b with
    | Some _, Some y -> Some y
    | _ -> Option.None

let fMask a b =
    match a, b with
    | Option.None, Some y -> Some y
    | _ -> Option.None

let fUpdateCount iter a b =
    match a, b with
    | Option.None, Option.None -> Option.None
    | Option.None, Some _ -> Some iter
    | Some x, Option.None -> Some x
    | Some x, Some _ -> Some x

let markVertices lst value = List.map (fun x -> x, value) lst

let BFS parallelLevel startingVertices (graph: Graph<'A>) =

    let numVertices = graph.AdjMtx.Rows

    let frontier = SparseVector(markVertices startingVertices (Some()), numVertices)

    let visited = SparseVector(markVertices startingVertices 0u, numVertices)

    let rec inner (frontier: SparseVector<Option<unit>>) visited counter =

        if frontier.IsEmpty then
            visited
        else

            let newFrontier =
                MatrixAlgebra.vecByMtx parallelLevel fAdd fMult frontier graph.AdjMtx
                |> SparseVector.Map2 parallelLevel fMask visited

            let newVisited =
                SparseVector.Map2 parallelLevel (fUpdateCount counter) visited newFrontier

            inner newFrontier newVisited (counter + 1u)

    inner frontier visited 1u
