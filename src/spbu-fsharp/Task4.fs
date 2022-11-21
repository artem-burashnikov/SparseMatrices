namespace HomeWork4

open System
open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees


module VectorData =

    type Vector<'Value>(arr: 'Value option array, head: int, length: int) =
        struct
            member this.Memory = arr
            member this.Head = head
            member this.Length = length
            member this.DataMaxIndex = arr.Length - 1
        end



    /// Splits a given Vector in half.
    let vecDiv2 (vec: Vector<'Value>) =
        let newLength = vec.Length / 2
        Vector(vec.Memory, vec.Head, newLength), Vector(vec.Memory, vec.Head + newLength, newLength)



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
    let vecToTree (arr: array<'Value option>) =
        let rec maker (vec: Vector<'Value>) =
            // If a given vector's starting index is outside of bounds of "memory",
            // then there is no need to store anything in a binary tree.
            if vec.Head > vec.DataMaxIndex then
                BinTree.None
            // If we find a 1x1 cell that is within the bounds of the vector's "memory".
            // Then look at data in that cell and store it accordingly.
            elif vec.Length = 1 then
                vec.Memory[vec.Head] |> store
            // Otherwise split the vector in half
            // and recursively call the maker function on the resulting halves.
            else
                let leftPart, rightPart = vecDiv2 vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        let vec = Vector(arr, 0, arr.Length)

        // Special cases to save space in a resulting binary tree.
        // i.e. BinTree.Leaf('a) instead of BinTree.Node(BinTree.Leaf('a), BinTree.None)
        if vec.Length = 0 then
            BinTree.None
        elif (vec.Length = 1) then
            vec.Memory[vec.Head] |> store
        else
            // In general case we upsize a given vector's length to a power of 2 so that the data correctly gets split in half on each recursive call.
            // Result is a healthy binary tree.
            let powerSize = Numbers.ceilPowTwo arr.Length

            let vec = Vector(arr, 0, powerSize)
            maker vec



module SparseVector =

    type SparseVector<'Value when 'Value: equality>(tree: BinTree<'Value>, length: int) =

        new(arr) = SparseVector(VectorData.vecToTree arr, arr.Length)

        member this.Length = length

        member this.Data = tree

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
                        let middle = size / 2

                        if i < middle then
                            search i middle leftChild
                        else
                            search (i - middle) middle rightChild

                let getValue (i: int) =
                    if i < 0 || i >= this.Length then
                        failwith $"SparseVector.Item with get(i): Index %A{i} is out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo this.Length
                        search i powerSize this.Data

                getValue i



open SparseVector

module MatrixData =

    type Matrix<'Value>(arr2d: 'Value option[,], x: int, y: int, rows: int, columns: int) =
        struct
            member this.Memory = arr2d
            member this.HeadX = x
            member this.HeadY = y
            member this.Rows = rows
            member this.Columns = columns

            member this.MaxRowIndex = (Array2D.length1 arr2d) - 1

            member this.MaxColumnIndex = (Array2D.length2 arr2d) - 1
        end



    /// Splits a given table in 4 quadrants by middle-points.
    let mtxDiv4 (mtx: Matrix<'Value>) =
        if mtx.Rows = 0 || mtx.Columns = 0 then
            mtx, mtx, mtx, mtx
        else
            let halfRows = mtx.Rows / 2
            let halfColumns = mtx.Columns / 2
            //
            //         halfColumns
            //   |          |           |
            //   |    NW    |    NE     |
            //  _|__________|___________|_ halfRows
            //   |          |           |
            //   |    SW    |    SE     |
            //   |          |           |
            //
            Matrix(mtx.Memory, mtx.HeadX, mtx.HeadY, halfRows, halfColumns), // NW
            Matrix(mtx.Memory, mtx.HeadX, mtx.HeadY + halfColumns, halfRows, halfColumns), // NE
            Matrix(mtx.Memory, mtx.HeadX + halfRows, mtx.HeadY, halfRows, halfColumns), // SW
            Matrix(mtx.Memory, mtx.HeadX + halfRows, mtx.HeadY + halfColumns, halfRows, halfColumns) // SE



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
    let tableToTree (table: 'Value option[,]) =
        let rec maker (mtx: Matrix<'Value>) =
            // If we find a cell within bounds of the original data, then store the cell's value accordingly.
            if
                mtx.Rows = 1
                && mtx.Columns = 1
                && mtx.HeadX <= mtx.MaxRowIndex
                && mtx.HeadY <= mtx.MaxColumnIndex

            then
                mtx.Memory[mtx.HeadX, mtx.HeadY] |> store

            // If the Quadrant's top-left cell is not in the range of the original data, no need to look further.
            elif mtx.HeadX > mtx.MaxRowIndex || mtx.HeadY > mtx.MaxColumnIndex then
                QuadTree.None
            // Otherwise we partition the Quadrant again.
            else

                let nw, ne, sw, se = mtxDiv4 mtx

                QuadTree.Node(maker nw, maker ne, maker sw, maker se) |> reduce

        // Check a given table's dimensions and act accordingly.
        let rows, columns = Array2D.length1 table, Array2D.length2 table

        if rows < 0 || columns < 0 then
            failwith $"MatrixData.tableToTree: Incorrect table data: %A{table}, rows %A{rows}, columns %A{columns}"

        elif rows = 0 || columns = 0 then
            QuadTree.None
        elif rows = 1 && columns = 1 then
            table[0, 0] |> store

        else
            let powerSize = Numbers.ceilPowTwo (max rows columns)
            // Initial top-left cell starts at coordinates (0, 0).
            let mtx = Matrix(table, 0, 0, powerSize, powerSize)

            maker mtx



open MatrixData

module SparseMatrix =

    type SparseMatrix<'Value when 'Value: equality>(data: QuadTree<'Value>, rows: int, columns: int) =

        new(arr2d) = SparseMatrix(tableToTree arr2d, Array2D.length1 arr2d, Array2D.length2 arr2d)

        member this.Rows = rows
        member this.Columns = columns
        member this.Data = data

        member this.Item
            with get (i: int, j: int) =
                // Binary search the value at given indices.
                let rec search i j size tree =

                    match tree with
                    | QuadTree.Leaf value -> Some value
                    | QuadTree.None -> Option.None
                    | QuadTree.Node(nw, ne, sw, se) ->

                        let middle = size / 2

                        let newI, newJ, quadrant =
                            if i < middle && j < middle then (i, j, nw)
                            elif i < middle && j >= middle then (i, j - middle, ne)
                            elif i >= middle && j < middle then (i - middle, j, sw)
                            else (i - middle, j - middle, se)

                        search newI newJ middle quadrant

                let getValue i j =

                    if i < 0 || j < 0 || i >= this.Rows || j >= this.Columns then
                        failwith $"SparseMatrix.Item with get(i, j): Indices %A{(i, j)} out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo (max this.Rows this.Columns)
                        search i j powerSize this.Data
                // Since indices in programming start at 0 we offset by one
                getValue i j


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
    let vecPlusVec fAdd (vec1: SparseVector<'a>) (vec2: SparseVector<'b>) : SparseVector<'c> =

        let rec sum bTree1 bTree2 =
            match bTree1, bTree2 with
            | BinTree.None, BinTree.None -> BinTree.None

            | BinTree.None, BinTree.Leaf b -> fAdd Option.None (Some b) |> convertResult

            | BinTree.None, BinTree.Node(b1, b2) -> BinTree.Node(sum bTree1 b1, sum bTree1 b2) |> VectorData.reduce

            | BinTree.Leaf a, BinTree.None -> fAdd (Some a) Option.None |> convertResult

            | BinTree.Node(a1, a2), BinTree.None -> BinTree.Node(sum a1 bTree2, sum a2 bTree2) |> VectorData.reduce

            | BinTree.Leaf a, BinTree.Leaf b -> fAdd (Some a) (Some b) |> convertResult

            | BinTree.Leaf _, BinTree.Node(b1, b2) -> BinTree.Node(sum bTree1 b1, sum bTree1 b2) |> VectorData.reduce

            | BinTree.Node(a1, a2), BinTree.Leaf _ -> BinTree.Node(sum a1 bTree2, sum a2 bTree2) |> VectorData.reduce

            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) -> BinTree.Node(sum a1 b1, sum a2 b2) |> VectorData.reduce

        if vec1.Length <> vec2.Length then
            failwith "Dimensions of objects don't match."
        else
            SparseVector(sum vec1.Data vec2.Data, vec1.Length)



    /// Vector by Matrix multiplication.
    let vecByMtx
        fAdd
        (fMult: 'a option -> 'b option -> 'c option)
        (vec: SparseVector<'a>)
        (mtx: SparseMatrix<'b>)
        : SparseVector<'c> =


        // We need to calculate how deep is the deepest tree.
        // We do it by recalling how it was constructed.
        // Now and later we may only use number of Rows of a given matrix for necessary calculations,
        // since a given vector's length would have to match it to produce the result.
        let powerSize = (max mtx.Rows mtx.Columns) |> Numbers.ceilPowTwo
        let maxDepth = Numbers.powTwo powerSize

        // Multiplication optimization.
        // We need to calculate n sums and the first sum is already given, so the counter stops at 1 (starts at n).
        let rec multMult (value: 'c option) counter =
            if counter = 1 then
                value
            else
                let acc = fAdd value value
                multMult acc (counter - 1)


        // Multiplication.
        let rec mult bTree qTree depth =

            //
            //          [qt1]
            //          [qt2]
            //  [a1 a2]  *   = a1 * qt1 + a2 * qt2
            //
            let fDo a1 a2 qt1 qt2 depth =
                let vec1 = SparseVector((mult a1 qt1 (depth + 1)), mtx.Columns)
                let vec2 = SparseVector(mult a2 qt2 (depth + 1), mtx.Columns)
                let result = vecPlusVec fAdd vec1 vec2
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
                | BinTree.Node(left, _) -> chopChop (level + 1) size left
                | BinTree.Leaf _
                | BinTree.None -> bTree
            else
                bTree


        // Pad the much needed branches so the multiplication actually works as intended.
        let rec powerUp level size bTree =
            if level < size then
                match bTree with
                | BinTree.Node _ -> BinTree.Node(powerUp (level + 1) size bTree, BinTree.None)
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
        elif mtx.Rows = 1 && mtx.Columns = 1 then
            let tree = fMult vec[0] mtx[0, 0] |> convertResult

            SparseVector(tree, mtx.Columns)

        // In this case we would have to pad the vector's length before multiplication.
        elif mtxRowPower < mtxColumnPower then
            let tree = mult (powerUp 0 diff vec.Data) mtx.Data 0
            SparseVector(tree, mtx.Columns)

        // When sizes match no additional action is needed.
        elif mtxRowPower = mtxColumnPower then
            let tree = mult vec.Data mtx.Data 0
            SparseVector(tree, mtx.Columns)
        else
            // Otherwise we need to clean up the result by chopping.
            let tree = mult vec.Data mtx.Data 0 |> chopChop 0 diff
            SparseVector(tree, mtx.Columns)
