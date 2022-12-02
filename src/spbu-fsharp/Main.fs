namespace spbu_fsharp

open HomeWork5
open Microsoft.FSharp.Core


module Main =

    [<EntryPoint>]
    let main (argv: string array) =

        let inputCoord =
            [ (0, 4, 1)
              (0, 6, 1)
              (2, 4, 1)
              (4, 0, 1)
              (4, 2, 1)
              (4, 5, 1)
              (5, 4, 1)
              (6, 0, 1)
              (0, 2, 1)
              (2, 0, 1) ]

        let rows, columns = 7, 7
        let cooMtx = COOMatrix(inputCoord, rows, columns)
        let startV = [ 0; 5 ]
        let result = Graphs.BFS startV cooMtx
        printfn $"%A{result.Data}"
        0
