namespace HomeWork5

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Trees
open Trees.QuadTrees
open Trees.BinTrees
open Helpers


type COOMatrix<'a> =
    struct
        val Data: (int * int * 'a option) list
        val Rows: int
        val Columns: int

        new(triplesList, rows, columns) =
            { Data = triplesList
              Rows = rows
              Columns = columns }
    end


module Converter =

    let first (a, _, _) = a
    let second (_, a, _) = a
    let third (_, _, a) = a

    let mtxDiv4 (mtx: COOMatrix<'a>) =

        if mtx.Rows = 0 || mtx.Columns = 0 then
            mtx, mtx, mtx, mtx

        else
            let halfRows = mtx.Rows / 2
            let halfColumns = mtx.Columns / 2

            let q1 triple =
                (first triple) < halfRows && (second triple) < halfColumns

            let q2 triple =
                (first triple) < halfRows && (second triple) >= halfColumns

            let q3 triple =
                (first triple) >= halfRows && (second triple) < halfColumns

            let q4 triple =
                (first triple) >= halfRows && (second triple) >= halfColumns

            let q1List = List.filter q1 mtx.Data // NW

            let q2List =
                List.filter q2 mtx.Data
                |> List.map (fun (i, j, value) -> (i, j - halfColumns, value)) // NE

            let q3List =
                List.filter q3 mtx.Data
                |> List.map (fun (i, j, value) -> (i - halfRows, j, value)) // SW

            let q4List =
                List.filter q4 mtx.Data
                |> List.map (fun (i, j, value) -> (i - halfRows, j - halfColumns, value)) // SE

            COOMatrix(q1List, halfRows, halfColumns),
            COOMatrix(q2List, halfRows, halfColumns),
            COOMatrix(q3List, halfRows, halfColumns),
            COOMatrix(q4List, halfRows, halfColumns)


    let reduce quadTreeNode =
        match quadTreeNode with
        | QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
        | QuadTree.Node(QuadTree.Leaf nw, QuadTree.Leaf ne, QuadTree.Leaf sw, QuadTree.Leaf se) when
            nw = ne && nw = sw && nw = se
            ->
            QuadTree.Leaf nw
        | _ -> quadTreeNode


    let cooToTree (mtx: COOMatrix<'a>) =

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

            elif
                mtx.Data.Length < 1

            then
                QuadTree.None

            else

                let nw, ne, sw, se = mtxDiv4 mtx

                QuadTree.Node(maker nw, maker ne, maker sw, maker se) |> reduce

        // Check dimensions and act accordingly.
        if mtx.Rows < 0 || mtx.Columns < 0 then
            failwith $"MatrixData.tableToTree: Incorrect data: %A{mtx.Data}, rows %A{mtx.Rows}, columns %A{mtx.Columns}"

        elif mtx.Rows = 0 || mtx.Columns = 0 then
            QuadTree.None

        elif mtx.Rows = 1 && mtx.Columns = 1 && mtx.Data.Length <> 0 then
            QuadTree.Leaf(mtx.Data.Head |> third)

        else
            let powerSize = Numbers.ceilPowTwo (max mtx.Rows mtx.Columns)
            // Initial top-left cell starts at coordinates (0, 0).
            let mtx = COOMatrix(mtx.Data, powerSize, powerSize)

            maker mtx
