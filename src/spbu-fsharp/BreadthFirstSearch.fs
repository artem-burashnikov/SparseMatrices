namespace BreadthFirstSearch

open Microsoft.FSharp.Core

open MatrixAlgebra
open SparseVector.SparseVector
open SparseMatrix.SparseMatrix
open SparseMatrix.MatrixData

module BFS =

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


    let BFS (startV: List<uint>) (gMtx: COOMatrix<'A>) =

        let mtx = SparseMatrix gMtx
        let length = mtx.Rows

        let frontier = SparseVector(markVertices startV (Some()), length)

        let visited = SparseVector(markVertices startV 0u, length)

        let rec inner (frontier: SparseVector<Option<unit>>) visited counter =

            if frontier.IsEmpty then
                visited
            else

                let newFrontier =
                    MatrixAlgebra.vecByMtx fAdd fMult frontier mtx
                    |> MatrixAlgebra.elementwiseVecVec fMask visited

                let newVisited =
                    MatrixAlgebra.elementwiseVecVec (fUpdateCount counter) visited newFrontier

                inner newFrontier newVisited (counter + 1u)

        inner frontier visited 1u
