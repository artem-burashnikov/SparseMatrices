namespace SparseMatrix

open Microsoft.Diagnostics.Tracing.Parsers.Clr
open Microsoft.Diagnostics.Tracing.Parsers.Kernel
open Trees
open Trees.QuadTrees
open Helpers


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
                && (mtx.Data.Head |> GeneralFunction.takeFirst) <= maxRowIndex
                && (mtx.Data.Head |> GeneralFunction.takeSecond) <= maxColumnIndex
            then
                QuadTree.Leaf(GeneralFunction.takeThird mtx.Data.Head)
            elif mtx.Data.Length < 1 then
                QuadTree.None
            else
                let nw, ne, sw, se = cooMTXPartition mtx

                QuadTree.Node(inner nw, inner ne, inner sw, inner se) |> reduce

        if mtx.Rows = 0u || mtx.Columns = 0u then
            QuadTree.None
        elif mtx.Rows = 1u && mtx.Columns = 1u && mtx.Data.Length <> 0 then
            QuadTree.Leaf(GeneralFunction.takeThird mtx.Data.Head)
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

        static member Fold folder state (mtx: SparseMatrix<'A>) =

            let rec inner folder i j state size qTree =
                match qTree with
                | QuadTree.None -> state
                | QuadTree.Leaf value -> folder i j value state
                | QuadTree.Node(nw, ne, sw, se) ->
                    let half = size / 2u
                    let resultFromNW = inner folder i j state half nw
                    let resultFromNE = inner folder i (j + half) resultFromNW half ne
                    let resultFromSW = inner folder (i + half) j resultFromNE half sw
                    inner folder (i + half) (j + half) resultFromSW half se

            let size = Numbers.ceilPowTwo (max mtx.Rows mtx.Columns)
            inner folder 0u 0u state size mtx.Data
