namespace HomeWork4

open System
open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees

module VectorData =

    type ArrVector<'A>(arr: array<Option<'A>>, head: uint, length: uint) =
        struct
            member this.Memory = arr
            member this.Head = head
            member this.Length = length
            member this.DataMaxIndex = arr.Length - 1 |> uint
        end


    type COOVector<'A>(list: List<uint * 'A>, length: uint) =
        member this.Data = list
        member this.Length = length


    /// Splits a given Vector in half.
    let arrVecPartition (vec: ArrVector<'A>) =
        let newLength = vec.Length / 2u
        ArrVector(vec.Memory, vec.Head, newLength), ArrVector(vec.Memory, vec.Head + newLength, newLength)


    /// Divide a given COOVector into 2 parts.
    /// Returns two vectors.
    let cooVecPartition (vec: COOVector<'A>) =
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


    /// Reduces identical data in a node of a binary tree to save space.
    let reduce binTreeNode =
        match binTreeNode with
        | BinTree.Node(BinTree.None, BinTree.None) -> BinTree.None
        | BinTree.Node(BinTree.Leaf a1, BinTree.Leaf a2) when a1 = a2 -> BinTree.Leaf a1
        | _ -> binTreeNode


    /// Stores a value (if anything) in a binary tree's branch.
    let store =
        function
        | Some value -> BinTree.Leaf value
        | Option.None -> BinTree.None


    /// Converts an array to Vector.
    let arrVecToTree (arr: array<Option<'A>>) =
        let rec maker (vec: ArrVector<'A>) =
            // If a given vector's starting index is outside of bounds of "memory",
            // then there is no need to store anything in a binary tree.
            if vec.Head > vec.DataMaxIndex then
                BinTree.None
            // If we find a 1x1 cell that is within the bounds of the vector's "memory".
            // Then look at data in that cell and store it accordingly.
            elif vec.Length = 1u then
                let index = Convert.ToInt32 vec.Head
                vec.Memory[index] |> store
            // Otherwise split the vector in half
            // and recursively call the maker function on the resulting halves.
            else
                let leftPart, rightPart = arrVecPartition vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        let vec = ArrVector(arr, 0u, arr.Length |> uint)

        // Special cases to save space in a resulting binary tree.
        // i.e. BinTree.Leaf('a) instead of BinTree.Node(BinTree.Leaf('a), BinTree.None)
        if vec.Length = 0u then
            BinTree.None
        elif (vec.Length = 1u) then
            let index = Convert.ToInt32 vec.Head
            vec.Memory[index] |> store
        else
            // In general case we upsize a given vector's length to a power of 2 so that the data correctly gets split in half on each recursive call.
            // Result is a healthy binary tree.
            let powerSize = Numbers.ceilPowTwo (arr.Length |> uint)

            let vec = ArrVector(arr, 0u, powerSize)
            maker vec


    let cooVecToTree (vec: COOVector<'A>) =

        let maxDataIndex = vec.Length - 1u

        let rec maker (vec: COOVector<'A>) =

            if vec.Length = 1u && vec.Data.Length = 1 && (fst vec.Data.Head) <= maxDataIndex then
                BinTree.Leaf(snd vec.Data.Head)
            elif vec.Data.Length < 1 then
                BinTree.None
            else
                let leftPart, rightPart = cooVecPartition vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        if vec.Length = 0u then
            BinTree.None
        elif vec.Length = 1u && vec.Data.Length <> 0 then
            BinTree.Leaf(snd vec.Data.Head)
        else
            let powerSize = Numbers.ceilPowTwo vec.Length
            COOVector(vec.Data, powerSize) |> maker


open VectorData

module SparseVector =

    type SparseVector<'A when 'A: equality> =
        val Data: BinTree<'A>
        val Length: uint

        new(tree, length) = { Data = tree; Length = length }

        new(arr) =
            { Data = arrVecToTree arr
              Length = arr.Length |> uint }

        new(cooVec) =
            { Data = cooVecToTree cooVec
              Length = cooVec.Length }

        new(cooList, length) =
            { Data = COOVector(cooList, length) |> cooVecToTree
              Length = length }

        member this.Item
            with get i =

                // Given an index i (starts at 1) we want to find a corresponding data in a tree.
                // By recalling how the tree was constructed we recursively call each node's child according to
                // which part the i would fall into. Basically a binary search algorithm.
                //
                //     i      middle
                // ------------||------------
                //
                let rec search i size tree =
                    match tree with
                    | BinTree.Leaf value -> Some value
                    | BinTree.None -> Option.None
                    | BinTree.Node(leftChild, rightChild) ->
                        let middle = size / 2u

                        if i < middle then
                            search i middle leftChild
                        else
                            search (i - middle) middle rightChild

                let getValue i =
                    if i >= this.Length then
                        failwith $"SparseVector.Item with get(i): Index %A{i} is out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo this.Length
                        search i powerSize this.Data

                getValue i

        member this.IsEmpty = this.Data = BinTree.None


open SparseVector
open Helpers.GeneralFunction

module MatrixData =

    type TableMatrix<'A>(arr2d: Option<'A>[,], x: uint, y: uint, rows: uint, columns: uint) =
        struct
            member this.Memory = arr2d
            member this.HeadX = x
            member this.HeadY = y
            member this.Rows = rows
            member this.Columns = columns

            member this.MaxRowIndex = (Array2D.length1 arr2d) - 1 |> uint

            member this.MaxColumnIndex = (Array2D.length2 arr2d) - 1 |> uint
        end


    type COOMatrix<'A> =
        struct
            val Data: List<uint * uint * 'A>
            val Rows: uint
            val Columns: uint

            new(triplesList, rows, columns) =
                { Data = triplesList
                  Rows = rows
                  Columns = columns }
        end


    /// Splits a given table in 4 quadrants by middle-points.
    let tableMTXPartition (mtx: TableMatrix<'A>) =
        if mtx.Rows = 0u || mtx.Columns = 0u then
            mtx, mtx, mtx, mtx
        else
            let halfRows = mtx.Rows / 2u
            let halfColumns = mtx.Columns / 2u
            //
            //         halfColumns
            //   |          |           |
            //   |    NW    |    NE     |
            //  _|__________|___________|_ halfRows
            //   |          |           |
            //   |    SW    |    SE     |
            //   |          |           |
            //
            TableMatrix(mtx.Memory, mtx.HeadX, mtx.HeadY, halfRows, halfColumns), // NW
            TableMatrix(mtx.Memory, mtx.HeadX, mtx.HeadY + halfColumns, halfRows, halfColumns), // NE
            TableMatrix(mtx.Memory, mtx.HeadX + halfRows, mtx.HeadY, halfRows, halfColumns), // SW
            TableMatrix(mtx.Memory, mtx.HeadX + halfRows, mtx.HeadY + halfColumns, halfRows, halfColumns) // SE


    /// Divide a given COOMatrix into 4 quadrants.
    /// Returns four matrices.
    let cooMTXPartition (mtx: COOMatrix<'A>) =

        // For an empty matrix just return the data immediately.
        // Otherwise filter triplets.
        if mtx.Rows = 0u || mtx.Columns = 0u then
            mtx, mtx, mtx, mtx
        else
            // Calculate middle points and filter triplets by comparing
            // corresponding coordinates to their middle points.
            let halfRows = mtx.Rows / 2u
            let halfColumns = mtx.Columns / 2u

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


    /// Reduces identical data in a node of a quad tree to save space.
    let reduce quadTreeNode =
        match quadTreeNode with
        | QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
        | QuadTree.Node(QuadTree.Leaf nw, QuadTree.Leaf ne, QuadTree.Leaf sw, QuadTree.Leaf se) when
            nw = ne && nw = sw && nw = se
            ->
            QuadTree.Leaf nw
        | _ -> quadTreeNode


    /// Stores a value (if anything) in a quad tree's branch.
    let store =
        function
        | Some value -> QuadTree.Leaf value
        | Option.None -> QuadTree.None


    /// Converts a given table into a QudTree.
    let tableToTree (table: Option<'A>[,]) =
        let rec maker (mtx: TableMatrix<'A>) =
            // If we find a cell within bounds of the original data, then store the cell's value accordingly.
            if
                mtx.Rows = 1u
                && mtx.Columns = 1u
                && mtx.HeadX <= mtx.MaxRowIndex
                && mtx.HeadY <= mtx.MaxColumnIndex

            then
                let i = Numbers.toIntConv mtx.HeadX
                let j = Numbers.toIntConv mtx.HeadY
                mtx.Memory[i, j] |> store

            // If the Quadrant's top-left cell is not in the range of the original data, no need to look further.
            elif mtx.HeadX > mtx.MaxRowIndex || mtx.HeadY > mtx.MaxColumnIndex then
                QuadTree.None
            // Otherwise we partition the Quadrant again.
            else

                let nw, ne, sw, se = tableMTXPartition mtx

                QuadTree.Node(maker nw, maker ne, maker sw, maker se) |> reduce

        // Check a given table's dimensions and act accordingly.
        let rows, columns = Array2D.length1 table |> uint, Array2D.length2 table |> uint

        if rows = 0u || columns = 0u then
            QuadTree.None
        elif rows = 1u && columns = 1u then
            table[0, 0] |> store

        else
            let powerSize = Numbers.ceilPowTwo (max rows columns)
            // Initial top-left cell starts at coordinates (0, 0).
            let mtx = TableMatrix(table, 0u, 0u, powerSize, powerSize)

            maker mtx


    /// Convert SparseMatrix data from coordinates to QuadTree.
    let cooMtxToTree (mtx: COOMatrix<'A>) =

        let maxRowIndex = mtx.Rows - 1u
        let maxColumnIndex = mtx.Columns - 1u

        let rec inner (mtx: COOMatrix<'A>) =

            if
                mtx.Rows = 1u
                && mtx.Columns = 1u
                && mtx.Data.Length = 1
                && (mtx.Data.Head |> takeFirst) <= maxRowIndex
                && (mtx.Data.Head |> takeSecond) <= maxColumnIndex
            then
                QuadTree.Leaf(takeThird mtx.Data.Head)
            elif mtx.Data.Length < 1 then
                QuadTree.None
            else
                let nw, ne, sw, se = cooMTXPartition mtx

                QuadTree.Node(inner nw, inner ne, inner sw, inner se) |> reduce

        if mtx.Rows = 0u || mtx.Columns = 0u then
            QuadTree.None
        elif mtx.Rows = 1u && mtx.Columns = 1u && mtx.Data.Length <> 0 then
            QuadTree.Leaf(takeThird mtx.Data.Head)
        else
            let powerSize = Numbers.ceilPowTwo (max mtx.Rows mtx.Columns)
            COOMatrix(mtx.Data, powerSize, powerSize) |> inner



open MatrixData

module SparseMatrix =

    type SparseMatrix<'A when 'A: equality>(data: QuadTree<'A>, rows: uint, columns: uint) =

        new(arr2d) = SparseMatrix(tableToTree arr2d, Array2D.length1 arr2d |> uint, Array2D.length2 arr2d |> uint)
        new(cooMTX) = SparseMatrix(cooMtxToTree cooMTX, cooMTX.Rows, cooMTX.Columns)

        member this.Rows = rows
        member this.Columns = columns
        member this.Data = data

        member this.Item
            with get (i, j) =
                // Binary search the value at given indices.
                let rec search i j size tree =

                    match tree with
                    | QuadTree.Leaf value -> Some value
                    | QuadTree.None -> Option.None
                    | QuadTree.Node(nw, ne, sw, se) ->

                        let middle = size / 2u

                        let newI, newJ, quadrant =
                            if i < middle && j < middle then (i, j, nw)
                            elif i < middle && j >= middle then (i, j - middle, ne)
                            elif i >= middle && j < middle then (i - middle, j, sw)
                            else (i - middle, j - middle, se)

                        search newI newJ middle quadrant

                let getValue i j =

                    if i >= this.Rows || j >= this.Columns then
                        failwith $"SparseMatrix.Item with get(i, j): Indices %A{(i, j)} out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo (max this.Rows this.Columns)
                        search i j powerSize this.Data
                // Since indices in programming start at 0 we offset by one
                getValue i j

        member this.IsEmpty = this.Data = QuadTree.None


open SparseMatrix

module MatrixAlgebra =

    let getValue x =
        match x with
        | Some a -> a
        | _ -> failwith "Only accepts Some value"


    let convertResult value =
        match value with
        | Option.None -> BinTree.None
        | _ -> BinTree.Leaf(value |> getValue)


    /// Adds two Sparse vectors together.
    let elementwiseVecVec fDo (vec1: SparseVector<'A>) (vec2: SparseVector<'B>) : SparseVector<'C> =

        let rec inner bTree1 bTree2 =
            match bTree1, bTree2 with
            | BinTree.None, BinTree.None -> fDo Option.None Option.None |> convertResult

            | BinTree.None, BinTree.Leaf b -> fDo Option.None (Some b) |> convertResult

            | BinTree.None, BinTree.Node(b1, b2) -> BinTree.Node(inner bTree1 b1, inner bTree1 b2) |> VectorData.reduce

            | BinTree.Leaf a, BinTree.None -> fDo (Some a) Option.None |> convertResult

            | BinTree.Node(a1, a2), BinTree.None -> BinTree.Node(inner a1 bTree2, inner a2 bTree2) |> VectorData.reduce

            | BinTree.Leaf a, BinTree.Leaf b -> fDo (Some a) (Some b) |> convertResult

            | BinTree.Leaf _, BinTree.Node(b1, b2) ->
                BinTree.Node(inner bTree1 b1, inner bTree1 b2) |> VectorData.reduce

            | BinTree.Node(a1, a2), BinTree.Leaf _ ->
                BinTree.Node(inner a1 bTree2, inner a2 bTree2) |> VectorData.reduce

            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) -> BinTree.Node(inner a1 b1, inner a2 b2) |> VectorData.reduce

        if vec1.Length <> vec2.Length then
            failwith "Dimensions of objects don't match."
        else
            SparseVector(inner vec1.Data vec2.Data, vec1.Length)



    /// Vector by Matrix multiplication.
    let vecByMtx
        fAdd
        (fMult: Option<'A> -> Option<'B> -> Option<'C>)
        (vec: SparseVector<'A>)
        (mtx: SparseMatrix<'B>)
        : SparseVector<'C> =


        // We need to calculate how deep is the deepest tree.
        // We do it by recalling how it was constructed.
        // Now and later we may only use number of Rows of a given matrix for necessary calculations,
        // since a given vector's length would have to match it to produce the result.
        let powerSize = (max mtx.Rows mtx.Columns) |> Numbers.ceilPowTwo
        let maxDepth = Numbers.powTwo powerSize

        // Multiplication optimization.
        // We need to calculate n sums and the first sum is already given, so the counter stops at 1 (starts at n).
        let rec multMult value counter =
            if counter = 1u then
                value
            else
                let acc = fAdd value value
                multMult acc (counter - 1u)


        // Multiplication.
        let rec mult bTree qTree depth : BinTree<'C> =

            //
            //          [qt1]
            //          [qt2]
            //  [a1 a2]  *   = a1 * qt1 + a2 * qt2
            //
            let fDo a1 a2 qt1 qt2 depth =
                let vec1 = SparseVector((mult a1 qt1 (depth + 1u)), mtx.Columns)
                let vec2 = SparseVector(mult a2 qt2 (depth + 1u), mtx.Columns)
                let result = elementwiseVecVec fAdd vec1 vec2
                result.Data

            match bTree, qTree with
            // Neutral * Neutral
            | BinTree.None, _
            | _, QuadTree.None -> BinTree.None

            // Value * Value.
            // If both Leaves are met at the maximum depth, then just multiply values.
            // Otherwise the result is calculated over n summations,
            // where n is the difference between the maximum depth and the current level (starts at 0).
            | BinTree.Leaf a, QuadTree.Leaf b ->
                if depth = maxDepth then
                    fMult (Some a) (Some b) |> convertResult

                else
                    let result = fAdd (fMult (Some a) (Some b)) (fMult (Some a) (Some b))

                    match result with
                    | Option.None -> BinTree.None
                    | _ -> BinTree.Leaf(multMult result (maxDepth - depth) |> getValue)

            // BinTree.Leaf _ is actually BinTree.Node(a, a)
            | BinTree.Leaf _, QuadTree.Node(nw, ne, sw, se) ->
                BinTree.Node(fDo bTree bTree nw sw depth, fDo bTree bTree ne se depth)
                |> VectorData.reduce

            // QuadTree.Leaf _ is actually QuadTree.Node(b, b, b, b)
            | BinTree.Node(a1, a2), QuadTree.Leaf _ ->
                let result = fDo a1 a2 qTree qTree depth
                BinTree.Node(result, result) |> VectorData.reduce

            | BinTree.Node(a1, a2), QuadTree.Node(nw, ne, sw, se) ->
                BinTree.Node(fDo a1 a2 nw sw depth, fDo a1 a2 ne se depth) |> VectorData.reduce


        // How much wood would a woodchuck chuck if a woodchuck could chuck wood?
        // Answer: The size of a binTree.
        /// Chop unnecessary branches of the result of multiplication.
        let rec chopChop level size bTree =
            if level < size then
                match bTree with
                | BinTree.Node(left, _) -> chopChop (level + 1u) size left
                | BinTree.Leaf _
                | BinTree.None -> bTree
            else
                bTree


        // Pad the much needed branches so the multiplication actually works as intended.
        let rec powerUp level size bTree =
            if level < size then
                match bTree with
                | BinTree.Node _ -> BinTree.Node(powerUp (level + 1u) size bTree, BinTree.None)
                | BinTree.Leaf _
                | BinTree.None -> bTree
            else
                bTree


        // Calculate how many levels would we need to pad or to chop.
        // We then would start at 0 ang go up to that number.
        let mtxRowPower = mtx.Rows |> Numbers.powTwo
        let mtxColumnPower = mtx.Columns |> Numbers.powTwo
        let diff = (max mtxColumnPower mtxRowPower) - (min mtxRowPower mtxColumnPower)


        if vec.Length <> mtx.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{vec.Length} and Matrix rows %A{mtx.Rows} don't match."

        // 1x1 is an edge case.
        elif mtx.Rows = 1u && mtx.Columns = 1u then
            let tree = fMult vec[0u] mtx[0u, 0u] |> convertResult

            SparseVector(tree, mtx.Columns)

        // In this case we would have to pad the vector's length before multiplication.
        elif mtxRowPower < mtxColumnPower then
            let tree = mult (powerUp 0u diff vec.Data) mtx.Data 0u
            SparseVector(tree, mtx.Columns)

        // When sizes match no additional action is needed.
        elif mtxRowPower = mtxColumnPower then
            let tree = mult vec.Data mtx.Data 0u
            SparseVector(tree, mtx.Columns)
        else
            // Otherwise we need to clean up the result by chopping.
            let tree = mult vec.Data mtx.Data 0u |> chopChop 0u diff
            SparseVector(tree, mtx.Columns)
