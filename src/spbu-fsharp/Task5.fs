namespace HomeWork5

open HomeWork4.SparseVector
open HomeWork4.SparseMatrix
open HomeWork4.MatrixAlgebra
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees
open Helpers


type COOMatrix<'a> =
    struct
        val Data: List<int * int * 'a option>
        val Rows: int
        val Columns: int

        new(triplesList, rows, columns) =
            { Data = triplesList
              Rows = rows
              Columns = columns }
    end


type COOVector<'a> =
    struct
        val Data: List<int>
        val Length: int

        new(list, length) = { Data = list; Length = length }
    end


module Converter =

    let first (a, _, _) = a
    let second (_, a, _) = a
    let third (_, _, a) = a


    let reduceQt node =
        match node with
        | QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
        | QuadTree.Node(QuadTree.Leaf nw, QuadTree.Leaf ne, QuadTree.Leaf sw, QuadTree.Leaf se) when
            nw = ne && nw = sw && nw = se
            ->
            QuadTree.Leaf nw
        | _ -> node


    let reduceBt node =
        match node with
        | BinTree.Node(BinTree.None, BinTree.None) -> BinTree.None
        | BinTree.Node(BinTree.Leaf left, BinTree.Leaf right) when left = right -> BinTree.Leaf left
        | _ -> node


    /// Divide a given COOMatrix into 4 quadrants.
    /// Returns four matrices.
    let mtxDiv4 (mtx: COOMatrix<'a>) =

        // For an empty matrix just return the data immediately.
        // Otherwise filter triplets.
        if mtx.Rows = 0 || mtx.Columns = 0 then
            mtx, mtx, mtx, mtx

        else
            // Calculate middle points and filter triplets by comparing
            // corresponding coordinates to their middle points.
            let halfRows = mtx.Rows / 2
            let halfColumns = mtx.Columns / 2

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

            let nw, ne, sw, se = inner mtx.Data [] [] [] []

            COOMatrix(nw, halfRows, halfColumns),
            COOMatrix(ne, halfRows, halfColumns),
            COOMatrix(sw, halfRows, halfColumns),
            COOMatrix(se, halfRows, halfColumns)


    /// Divide a given COOVector into 2 parts.
    /// Returns two vectors.
    let vecDiv2 (vec: COOVector<'a>) =

        // For an empty vector just return the data immediately.
        // Otherwise filter coordinates.
        if vec.Length = 0 then
            vec, vec

        else
            // Calculate middle point and filter coordinates by comparing
            // them to the middle point.
            let half = vec.Length / 2

            let rec inner lst leftPart rightPart =
                match lst with
                | [] -> leftPart, rightPart
                | hd :: tl ->
                    if hd < half then
                        inner tl (hd :: leftPart) rightPart
                    else
                        inner tl leftPart ((hd - half) :: rightPart)

            let leftPart, rightPart = inner vec.Data [] []

            COOVector(leftPart, half), COOVector(rightPart, half)


    let cooMtxToTree (mtx: COOMatrix<'a>) =

        let maxRowIndex = mtx.Rows - 1
        let maxColumnIndex = mtx.Columns - 1

        let rec maker (mtx: COOMatrix<'a>) =

            if
                mtx.Rows = 1
                && mtx.Columns = 1
                && mtx.Data.Length = 1
                && (mtx.Data.Head |> first) <= maxRowIndex
                && (mtx.Data.Head |> second) <= maxColumnIndex
            then
                QuadTree.Leaf(mtx.Data.Head |> third)
            // match third mtx.Data.Head with
            // | Option.None -> QuadTree.None
            // | Some value -> QuadTree.Leaf value

            elif
                mtx.Data.Length < 1

            then
                QuadTree.None

            else

                let nw, ne, sw, se = mtxDiv4 mtx

                QuadTree.Node(maker nw, maker ne, maker sw, maker se) |> reduceQt

        if mtx.Rows < 0 || mtx.Columns < 0 then
            failwith $"Converter.cooMtxToTree: Incorrect data: %A{mtx.Data}, rows %A{mtx.Rows}, columns %A{mtx.Columns}"

        elif mtx.Rows = 0 || mtx.Columns = 0 then
            QuadTree.None

        elif mtx.Rows = 1 && mtx.Columns = 1 && mtx.Data.Length <> 0 then
            QuadTree.Leaf(mtx.Data.Head |> third)
        // match third mtx.Data.Head with
        // | Option.None -> QuadTree.None
        // | Some value -> QuadTree.Leaf value
        else
            let powerSize = Numbers.ceilPowTwo (max mtx.Rows mtx.Columns)
            COOMatrix(mtx.Data, powerSize, powerSize) |> maker


    let cooVecToTree (vec: COOVector<'a>) =

        let maxDataIndex = vec.Length - 1

        let rec maker (vec: COOVector<'a>) =

            if vec.Length = 1 && vec.Data.Length = 1 && vec.Data.Head <= maxDataIndex then

                BinTree.Leaf true

            elif
                vec.Data.Length < 1

            then
                BinTree.None

            else

                let leftPart, rightPart = vecDiv2 vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduceBt

        if vec.Length < 0 then
            failwith $"Converter.cooVecToTree: Incorrect data: %A{vec.Data}, length %A{vec.Length}"

        elif vec.Length = 0 then
            BinTree.None

        elif vec.Length = 1 && vec.Data.Length <> 0 then
            BinTree.Leaf true

        else
            let powerSize = Numbers.ceilPowTwo vec.Length
            COOVector(vec.Data, powerSize) |> maker


module Graphs =

    let applyMask (mask: SparseVector<'a>) (vec: SparseVector<'b>) =

        let rec inner maskTree vecTree =
            match maskTree, vecTree with
            | BinTree.None, BinTree.Leaf b -> BinTree.Leaf b

            | BinTree.None, vecTree -> vecTree

            | _, BinTree.None -> BinTree.None

            | BinTree.Leaf _, _ -> BinTree.None

            | BinTree.Node(a1, a2), BinTree.Leaf _ ->
                BinTree.Node(inner a1 vecTree, inner a2 vecTree) |> Converter.reduceBt

            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) -> BinTree.Node(inner a1 b1, inner a2 b2) |> Converter.reduceBt

        SparseVector(inner mask.Data vec.Data, mask.Length)


    let markVisited (visited: SparseVector<int>) (frontier: SparseVector<'b>) (iter: int) =

        let rec inner visitedTree frontTree =
            match visitedTree, frontTree with
            | BinTree.None, BinTree.Leaf _ -> BinTree.Leaf iter

            | BinTree.None, BinTree.Node(b1, b2) ->
                BinTree.Node(inner visitedTree b1, inner visitedTree b2) |> Converter.reduceBt

            | visitedTree, BinTree.None -> visitedTree

            | BinTree.Leaf _, BinTree.Node(b1, b2) ->
                BinTree.Node(inner visitedTree b1, inner visitedTree b2) |> Converter.reduceBt

            | BinTree.Node(a1, a2), BinTree.Leaf _ ->
                BinTree.Node(inner a1 frontTree, inner a2 frontTree) |> Converter.reduceBt

            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) -> BinTree.Node(inner a1 b1, inner a2 b2) |> Converter.reduceBt

            | BinTree.Leaf _, BinTree.Leaf _ ->
                failwith
                    "markVisited: frontier should already be masked at this point.\
                          This case is impossible."

        SparseVector(inner visited.Data frontier.Data, visited.Length)

(*
    let BFS (vSet: List<int>) (gMtx: COOMatrix<'a>) =

        let fAdd a b =
            match a, b with
            | Some true, _
            | _, Some true -> Some true
            | _ -> Some false

        let fMult a b =
            match a, b with
            | Some true, Some _ -> Some true
            | _ -> Some false

        // Initialize a frontier from a given list of starting vertices and make a BinaryTree.
        // After which construct a SparseVector from resulting Binary Tree.
        let rec initFrontier frontier vSet =
            match vSet with
            | [] -> frontier
            | [ vertex ] -> (vertex, true) :: frontier
            | hd :: tl -> initFrontier ((hd, true) :: frontier) tl

        let vecData =
            COOVector(initFrontier List.empty vSet, gMtx.Rows) |> Converter.cooVecToTree

        let frontier = SparseVector(vecData, gMtx.Rows)

        // Construct a SparseMatrix from a given adjacency matrix.
        let mtxData = Converter.cooMtxToTree gMtx
        let mtx = SparseMatrix(mtxData, gMtx.Rows, gMtx.Columns)

        // Mark given vertices as visited. This counts as an iteration 0.
        let visited = markVisited (SparseVector(BinTree.None, frontier.Length)) frontier 0

        let rec innerBFS (frontier: SparseVector<bool>) (visited: SparseVector<int>) iter =
            // Follow adjacency matrix to get a new front.
            // By applying the mask of already visited vertices we will prevent cycling around.
            let newFrontier = applyMask visited (vecByMtx fAdd fMult frontier mtx)

            // If the resulting front is empty, then return already visited vertices.
            // Otherwise mark the newly visited vertices and continue BFS.
            if newFrontier.Data = BinTree.None then
                visited
            else
                let newVisited = markVisited visited newFrontier iter

                innerBFS newFrontier newVisited (iter + 1)

        if frontier.Data = BinTree.None then
            visited
        else
            innerBFS frontier visited 1
*)
