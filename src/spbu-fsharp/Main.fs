namespace spbu_fsharp

open HomeWork4.MatrixData
open Microsoft.FSharp.Core

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let arr =
            [| [| Some 1; Some 1 |]
               [| Some 1; Some 1 |] |]

        let mtx = mtxToTree arr
        printfn $"%A{mtx}"
        0
