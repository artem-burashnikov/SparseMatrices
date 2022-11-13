namespace spbu_fsharp

open HomeWork4
open Microsoft.FSharp.Core

module Main =

    [<EntryPoint>]
    let main (argv: string array) =


        let mult (table1: int[,]) (table2: int[,]) =
            let rows1 = Array2D.length1 table1
            let columns1 = Array2D.length2 table1
            let rows2 = Array2D.length1 table2
            let columns2 = Array2D.length2 table2

            let mutable result = Array2D.zeroCreate rows1 columns2

            for i = 0 to rows1 - 1 do
                for j = 0 to columns2 - 1 do
                    for k = 0 to rows2 - 1 do
                        result[i, j] <- result[i, j] + table1[i, k] * table2[k, j]

            result

        let arr = [| 1; 1; 1 |]

        let table = Array2D.init 3 2 (fun i j -> (i + j))



        let multiply (arr: array<int>) (table: int[,]) =
            let rows = arr.Length
            let columns = Array2D.length2 table
            let mutable result = Array.zeroCreate columns

            for j = 0 to columns - 1 do
                for i = 0 to rows - 1 do
                    result[j] <- result[j] + arr[i] * table[i, j]

            result

        let result = multiply arr table
        // let expectedResult = Array.map Some result |> SparseVector.SparseVector


        let vec = Array.map Some arr |> SparseVector.SparseVector
        let mtx = Array2D.map Some table |> SparseMatrix.SparseMatrix
        let actualResult = MatrixAlgebra.vecByMtx (+) (*) vec mtx

        printfn $"vec input %A{arr}"
        printfn $"mtx1 representation %A{vec.Data}"
        printfn "-----------------------------------------------------------"
        printfn $"matrix input %A{table}"
        printfn $"matrix representation %A{mtx.Data}"
        printfn "-----------------------------------------------------------"
        printfn $"naive mult = %A{result}"
        printfn $"vec mtx mult = %A{actualResult}"


        0
