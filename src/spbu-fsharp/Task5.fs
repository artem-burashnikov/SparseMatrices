namespace HomeWork5



type COOMatrix<'a> =
    struct
        val Data: (int * int * 'a) list
        val Rows: int
        val Columns: int

        new(triplesList, rows, columns) =
            { Data = triplesList
              Rows = rows
              Columns = columns }
    end



module Converter =


    open Helpers
    open Trees.QuadTrees


    let first (a, _, _) = a
    let second (_, a, _) = a
    let third (_, _, a) = a


    /// Divide a given COOMatrix into 4 quadrants.
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

            // NW
            let isQ1 triple =
                (first triple) < halfRows && (second triple) < halfColumns

            let q1List = List.filter isQ1 mtx.Data
            // NE
            let isQ2 triple =
                (first triple) < halfRows && (second triple) >= halfColumns

            let q2List =
                List.filter isQ2 mtx.Data
                |> List.map (fun (i, j, value) -> (i, j - halfColumns, value))
            // SW
            let isQ3 triple =
                (first triple) >= halfRows && (second triple) < halfColumns

            let q3List =
                List.filter isQ3 mtx.Data
                |> List.map (fun (i, j, value) -> (i - halfRows, j, value))
            // SE
            let isQ4 triple =
                (first triple) >= halfRows && (second triple) >= halfColumns

            let q4List =
                List.filter isQ4 mtx.Data
                |> List.map (fun (i, j, value) -> (i - halfRows, j - halfColumns, value))

            COOMatrix(q1List, halfRows, halfColumns),
            COOMatrix(q2List, halfRows, halfColumns),
            COOMatrix(q3List, halfRows, halfColumns),
            COOMatrix(q4List, halfRows, halfColumns)


    let reduce node =
        match node with
        | Node(None, None, None, None) -> None
        | Node(Leaf nw, Leaf ne, Leaf sw, Leaf se) when nw = ne && nw = sw && nw = se -> Leaf nw
        | _ -> node


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
            COOMatrix(mtx.Data, powerSize, powerSize) |> maker
