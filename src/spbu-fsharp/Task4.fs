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
            else if vec.Length = 1 then
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

                        if i <= middle then
                            search i middle leftChild
                        else
                            search (i - middle) middle rightChild

                let getValue (i: int) =
                    if i < 1 || i > this.Length then
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
                // Binary search the value at a given indices.
                let rec search (i, j) size tree =

                    match tree with
                    | QuadTree.Leaf value -> Some value
                    | QuadTree.None -> Option.None
                    | QuadTree.Node(nw, ne, sw, se) ->

                        let middle = size / 2

                        let newI, newJ, quadrant =
                            if i <= middle && j <= middle then (i, j, nw)
                            elif i <= middle && j > middle then (i, j - middle, ne)
                            elif i > middle && j <= middle then (i - middle, j, sw)
                            else (i - middle, j - middle, se)

                        search (newI, newJ) middle quadrant

                let getValue (i, j) =

                    if i < 1 || j < 1 || i > this.Rows || j > this.Columns then
                        failwith $"SparseMatrix.Item with get(i, j): Indices %A{(i, j)} out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo (max this.Rows this.Columns)
                        search (i, j) powerSize this.Data

                getValue (i, j)

open SparseMatrix

module MatrixAlgebra =

    /// Vector by Matrix multiplication.
    let vecByMtx fAdd (fMult: 'a -> 'b -> 'c) (vec: SparseVector<'a>) (mtx: SparseMatrix<'b>) =

        // Since data is stored as a Tree, we need to calculate the maximum depth.
        // This calculation will allow to make correct assumptions about what operations should we do when a Leaf is reached by recursion.
        // Multiplication and addition only occurs when the maximum depth is reached.
        // Otherwise we make a recursive call with an increased depth counter.
        let powerSize = max (max vec.Length mtx.Rows) mtx.Columns |> Numbers.ceilPowTwo
        let maxDepth = Math.Log2(float powerSize) |> int

        let rec sum binTree1 binTree2 currDepth =
            match binTree1, binTree2 with
            | BinTree.None, tree
            | tree, BinTree.None -> tree
            | BinTree.Leaf a, BinTree.Leaf b ->
                if currDepth = maxDepth then
                    BinTree.Leaf(fAdd a b)
                else
                    sum binTree1 binTree2 (currDepth + 1)
            | BinTree.Leaf _, BinTree.Node(b1, b2) ->
                let s1 = sum binTree1 b1 (currDepth + 1)
                let s2 = sum binTree1 b2 (currDepth + 1)
                BinTree.Node(s1, s2) |> VectorData.reduce
            | BinTree.Node(a1, a2), BinTree.Leaf _ ->
                let s1 = sum a1 binTree2 (currDepth + 1)
                let s2 = sum a2 binTree2 (currDepth + 1)
                BinTree.Node(s1, s2) |> VectorData.reduce
            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) ->
                let s1 = sum a1 b1 (currDepth + 1)
                let s2 = sum a2 b2 (currDepth + 1)
                BinTree.Node(s1, s2) |> VectorData.reduce


        // We only increase the depth level when the recursive call is made.
        // Because the sum function is called on the current depth level, the counter for sum does not increase.
        let rec mult binTree quadTree currDepth =
            match binTree, quadTree with
            // Neutral * Value = Neutral
            | BinTree.None, _
            | _, QuadTree.None -> BinTree.None
            | BinTree.Leaf a, QuadTree.Leaf b ->
                if currDepth = maxDepth then
                    BinTree.Leaf(fMult a b)
                else
                    sum (mult binTree quadTree (currDepth + 1)) (mult binTree quadTree (currDepth + 1)) currDepth
            // This is the case of BinTree.Node(a, a), QuadTree.Node(b1, b2, b3, b4)
            | BinTree.Leaf _, QuadTree.Node(b1, b2, b3, b4) ->
                let s1 =
                    sum (mult binTree b1 (currDepth + 1)) (mult binTree b3 (currDepth + 1)) currDepth

                let s2 =
                    sum (mult binTree b2 (currDepth + 1)) (mult binTree b4 (currDepth + 1)) currDepth

                BinTree.Node(s1, s2) |> VectorData.reduce
            // This is the case of BinTree.Node(a1, a2), QuadTree.Node(b, b, b, b)
            | BinTree.Node(a1, a2), QuadTree.Leaf _ ->
                let s1 =
                    sum (mult a1 quadTree (currDepth + 1)) (mult a2 quadTree (currDepth + 1)) currDepth

                let s2 =
                    sum (mult a1 quadTree (currDepth + 1)) (mult a2 quadTree (currDepth + 1)) currDepth

                BinTree.Node(s1, s2) |> VectorData.reduce
            // General case.
            | BinTree.Node(a1, a2), QuadTree.Node(b1, b2, b3, b4) ->
                let s1 = sum (mult a1 b1 (currDepth + 1)) (mult a2 b3 (currDepth + 1)) currDepth
                let s2 = sum (mult a1 b2 (currDepth + 1)) (mult a2 b4 (currDepth + 1)) currDepth
                BinTree.Node(s1, s2) |> VectorData.reduce

        if vec.Length <> mtx.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{vec.Length} and Matrix rows %A{mtx.Rows} don't match."

        else
            SparseVector(mult vec.Data mtx.Data 0, mtx.Columns)


    /// Matrix by Matrix multiplication.
    let mtxByMtx fAdd (fMult: 'a -> 'b -> 'c) (mtx1: SparseMatrix<'a>) (mtx2: SparseMatrix<'b>) =

        // Just to be sure upsize THE maximum.
        let powerSize =
            max (max mtx1.Rows mtx1.Columns) (max mtx2.Rows mtx2.Columns)
            |> Numbers.ceilPowTwo

        // Same idea. Calculate overall data structure depth and check a current level when a Leaf is reached.
        let maxDepth = Math.Log2(float powerSize) |> int

        let rec sum quadTree1 quadTree2 currDepth =
            match quadTree1, quadTree2 with
            // Neutral + Value = Value
            | QuadTree.None, tree
            | tree, QuadTree.None -> tree
            | QuadTree.Leaf a, QuadTree.Leaf b ->
                if currDepth = maxDepth then
                    QuadTree.Leaf(fAdd a b)
                else
                    sum quadTree1 quadTree2 (currDepth + 1)
            // The following two cases are of QuadTree.Node(a1, a2, a3, a4), QuadTree.Node(b1, b2, b3, b4)
            | QuadTree.Leaf _, QuadTree.Node(b1, b2, b3, b4) ->
                let s1 = sum quadTree1 b1 (currDepth + 1)
                let s2 = sum quadTree1 b2 (currDepth + 1)
                let s3 = sum quadTree1 b3 (currDepth + 1)
                let s4 = sum quadTree1 b4 (currDepth + 1)
                QuadTree.Node(s1, s2, s3, s4) |> reduce
            | QuadTree.Node(a1, a2, a3, a4), QuadTree.Leaf _ ->
                let s1 = sum a1 quadTree2 (currDepth + 1)
                let s2 = sum a2 quadTree2 (currDepth + 1)
                let s3 = sum a3 quadTree2 (currDepth + 1)
                let s4 = sum a4 quadTree2 (currDepth + 1)
                QuadTree.Node(s1, s2, s3, s4) |> reduce
            // General case.
            | QuadTree.Node(a1, a2, a3, a4), QuadTree.Node(b1, b2, b3, b4) ->
                let s1 = sum a1 b1 (currDepth + 1)
                let s2 = sum a2 b2 (currDepth + 1)
                let s3 = sum a3 b3 (currDepth + 1)
                let s4 = sum a4 b4 (currDepth + 1)
                QuadTree.Node(s1, s2, s3, s4) |> reduce


        let rec mult quadTree1 quadTree2 currDepth =
            match quadTree1, quadTree2 with
            // Neutral * Value = Neutral
            | QuadTree.None, _
            | _, QuadTree.None -> QuadTree.None
            | QuadTree.Leaf a, QuadTree.Leaf b ->
                if currDepth = maxDepth then
                    QuadTree.Leaf(fMult a b)
                else
                    // Note that depth level only increases for the mult function but not for the sum function.
                    sum (mult quadTree1 quadTree2 (currDepth + 1)) (mult quadTree1 quadTree2 (currDepth + 1)) currDepth
                    |> reduce
            // The following two cases are of QuadTree.Node(a1, a2, a3, a4), QuadTree.Node(b1, b2, b3, b4)
            | QuadTree.Leaf _, QuadTree.Node(b1, b2, b3, b4) ->
                let s1 =
                    sum (mult quadTree1 b1 (currDepth + 1)) (mult quadTree1 b3 (currDepth + 1)) currDepth

                let s2 =
                    sum (mult quadTree1 b2 (currDepth + 1)) (mult quadTree1 b4 (currDepth + 1)) currDepth
                // let sw = sum (mult quadTree1 b1) (mult quadTree1 b3)
                // let se = sum (mult quadTree1 b2) (mult quadTree1 b4)
                QuadTree.Node(s1, s2, s1, s2) |> reduce
            | QuadTree.Node(a1, a2, a3, a4), QuadTree.Leaf _ ->
                let nw =
                    sum (mult a1 quadTree2 (currDepth + 1)) (mult a2 quadTree2 (currDepth + 1)) currDepth

                let ne =
                    sum (mult a1 quadTree2 (currDepth + 1)) (mult a2 quadTree2 (currDepth + 1)) currDepth

                let sw =
                    sum (mult a3 quadTree2 (currDepth + 1)) (mult a4 quadTree2 (currDepth + 1)) currDepth

                let se =
                    sum (mult a3 quadTree2 (currDepth + 1)) (mult a4 quadTree2 (currDepth + 1)) currDepth

                QuadTree.Node(nw, ne, sw, se) |> reduce
            // General case.
            | QuadTree.Node(a1, a2, a3, a4), QuadTree.Node(b1, b2, b3, b4) ->
                let nw = sum (mult a1 b1 (currDepth + 1)) (mult a2 b3 (currDepth + 1)) currDepth
                let ne = sum (mult a1 b2 (currDepth + 1)) (mult a2 b4 (currDepth + 1)) currDepth
                let sw = sum (mult a3 b1 (currDepth + 1)) (mult a4 b3 (currDepth + 1)) currDepth
                let se = sum (mult a3 b2 (currDepth + 1)) (mult a4 b4 (currDepth + 1)) currDepth
                QuadTree.Node(nw, ne, sw, se) |> reduce

        if mtx1.Columns <> mtx2.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{mtx1.Columns} and Matrix rows %A{mtx2.Rows} don't match."

        else
            SparseMatrix(mult mtx1.Data mtx2.Data 0, mtx1.Rows, mtx2.Columns)
