namespace spbu_fsharp

open HomeWork5
open Microsoft.FSharp.Core


module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let listTuples = [ (0, 0, Some 4); (2, 1, Some 1); (3, 7, Some 9) ]
        let size = 10

        let func tupleLst (arr: 'a[,]) =
            let rec maker lst =
                match lst with
                | [] -> arr
                | (i, j, value) :: tl ->
                    arr[i, j] <- value
                    maker tl

            maker tupleLst

        let inputArr = func listTuples (Array2D.create size size Option.None)
        let quadFromCoo = COOMatrix(listTuples, size, size)
        let actualResult = Converter.cooMtxToTree quadFromCoo
        let expectedResult = HomeWork4.MatrixData.tableToTree inputArr
        printfn $"Input list: %A{listTuples}"
        printfn $"Input arr: %A{inputArr}"

        printfn
            $"Actual result:\n%A{actualResult}\n--------------------------------------------------------------------------\nExpected result:\n%A{expectedResult}"

        0
