namespace HomeWork5

open HomeWork4
open HomeWork4.SparseMatrix
open HomeWork4.SparseVector
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees
open Helpers


type COOMatrix<'A> =
    struct
        val Data: List<uint * uint * Option<'A>>
        val Rows: uint
        val Columns: uint

        new(triplesList, rows, columns) =
            { Data = triplesList
              Rows = rows
              Columns = columns }
    end


type COOVector<'A>(list: List<uint * Option<'A>>, length: uint) =
    member this.Data = list
    member this.Length = length

    static member CreateFromListOfIndices (lst: List<uint>) length (value: Option<'A>) =
        COOVector(List.map (fun x -> (x, value)) lst, length)


module Converter =

    let first (a, _, _) = a
    let second (_, a, _) = a
    let third (_, _, a) = a


    /// Divide a given COOMatrix into 4 quadrants.
    /// Returns four matrices.
    let mtxDiv4 (mtx: COOMatrix<'A>) =

        // For an empty matrix just return the data immediately.
        // Otherwise filter triplets.
        if mtx.Rows = 0u || mtx.Columns = 0u then
            mtx, mtx, mtx, mtx
        else
            // Calculate middle points and filter triplets by comparing
            // corresponding coordinates to their middle points.
            let halfRows = mtx.Rows / 2u
            let halfColumns = mtx.Columns / 2u

            //
            let rec inner lst nw ne sw se =
                match lst with
                | [] -> nw, ne, sw, se
                | (i, j, value) :: tl ->
                    if i < halfRows && j < halfColumns then
                        inner tl ((i, j, value) :: nw) ne sw se
                    elif i < halfRows && j >= halfColumns then
                        inner tl nw ((i, j - halfColumns, value) :: ne) sw se
                    elif i >= halfRows && j < halfColumns then
                        inner tl nw ne ((i - halfRows, j, value) :: sw) se
                    else
                        inner tl nw ne sw ((i - halfRows, j - halfRows, value) :: se)

            // Construct and return the result.
            let nw, ne, sw, se = inner mtx.Data [] [] [] []

            COOMatrix(nw, halfRows, halfColumns),
            COOMatrix(ne, halfRows, halfColumns),
            COOMatrix(sw, halfRows, halfColumns),
            COOMatrix(se, halfRows, halfColumns)


    /// Divide a given COOVector into 2 parts.
    /// Returns two vectors.
    let vecDiv2 (vec: COOVector<'A>) =
        // For an empty vector just return the data immediately.
        // Otherwise filter coordinates.
        if vec.Length = 0u then
            vec, vec
        else
            // Calculate middle point and filter coordinates by comparing
            // them to the middle point.
            let half = vec.Length / 2u

            let rec inner lst leftPart rightPart =
                match lst with
                | [] -> leftPart, rightPart
                | (i, value) :: tl ->
                    if i < half then
                        inner tl ((i, value) :: leftPart) rightPart
                    else
                        inner tl leftPart ((i - half, value) :: rightPart)

            // Construct and return the result.
            let leftPart, rightPart = inner vec.Data [] []

            COOVector(leftPart, half), COOVector(rightPart, half)


    /// Convert SparseMatrix data from coordinates to QuadTree.
    let cooMtxToTree (mtx: COOMatrix<'A>) =

        let maxRowIndex = mtx.Rows - 1u
        let maxColumnIndex = mtx.Columns - 1u

        let rec inner (mtx: COOMatrix<'A>) =

            if
                mtx.Rows = 1u
                && mtx.Columns = 1u
                && mtx.Data.Length = 1
                && (mtx.Data.Head |> first) <= maxRowIndex
                && (mtx.Data.Head |> second) <= maxColumnIndex
            then
                QuadTree.Leaf(third mtx.Data.Head)
            elif mtx.Data.Length < 1 then
                QuadTree.None
            else
                let nw, ne, sw, se = mtxDiv4 mtx

                QuadTree.Node(inner nw, inner ne, inner sw, inner se) |> MatrixData.reduce

        if mtx.Rows = 0u || mtx.Columns = 0u then
            QuadTree.None
        elif mtx.Rows = 1u && mtx.Columns = 1u && mtx.Data.Length <> 0 then
            QuadTree.Leaf(third mtx.Data.Head)
        else
            let powerSize = Numbers.ceilPowTwo (max mtx.Rows mtx.Columns)
            COOMatrix(mtx.Data, powerSize, powerSize) |> inner


    let cooVecToTree (vec: COOVector<'A>) =

        let maxDataIndex = vec.Length - 1u

        let rec maker (vec: COOVector<'A>) =

            if vec.Length = 1u && vec.Data.Length = 1 && (fst vec.Data.Head) <= maxDataIndex then
                BinTree.Leaf(snd vec.Data.Head)
            elif vec.Data.Length < 1 then
                BinTree.None
            else
                let leftPart, rightPart = vecDiv2 vec

                BinTree.Node(maker leftPart, maker rightPart) |> VectorData.reduce

        if vec.Length = 0u then
            BinTree.None
        elif vec.Length = 1u && vec.Data.Length <> 0 then
            BinTree.Leaf(snd vec.Data.Head)
        else
            let powerSize = Numbers.ceilPowTwo vec.Length
            COOVector(vec.Data, powerSize) |> maker


    let cooToSparseVec (cooVec: COOVector<'A>) =
        SparseVector(cooVecToTree cooVec, cooVec.Length)


    let cooToSparseMtx (cooMtx: COOMatrix<'A>) =
        SparseMatrix(cooMtxToTree cooMtx, cooMtx.Rows, cooMtx.Columns)


module Graphs =

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
        | Option.None, Some _ -> Some(Some iter)
        | Some x, Option.None -> Some x
        | Some x, Some _ -> Some x


    let BFS (startV: List<uint>) (gMtx: COOMatrix<'A>) =

        // Size and adjacency matrix.
        let length = gMtx.Rows
        let mtx = gMtx |> Converter.cooToSparseMtx

        // Currently visited vertices.
        let frontier =
            COOVector.CreateFromListOfIndices startV length (Some())
            |> Converter.cooToSparseVec

        // The result with weights.
        let visited =
            COOVector.CreateFromListOfIndices startV length (Some 0u)
            |> Converter.cooToSparseVec

        let rec inner frontier visited counter =

            let newFrontier =
                MatrixAlgebra.vecByMtx fAdd fMult frontier mtx
                |> MatrixAlgebra.elementwiseVecVec fMask visited

            if newFrontier.IsEmpty then
                visited
            else
                let newVisited =
                    MatrixAlgebra.elementwiseVecVec (fUpdateCount counter) visited newFrontier

                inner newFrontier newVisited (counter + 1u)

        if frontier.IsEmpty then
            visited
        else
            inner frontier visited 1u
