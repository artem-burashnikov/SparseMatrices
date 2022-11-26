namespace spbu_fsharp

open HomeWork5
open Microsoft.FSharp.Core
open HomeWork5


module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let lst =
            [ (4, 3, Some 3)
              (1, 3, Some 2)
              (1, 4, Some 2)
              (5, 7, Some 5)
              (9, 9, Some 9) ]

        let rows = 16
        let columns = 16
        let input = COOMatrix(lst, columns, columns)
        let res = Converter.cooToTree input
        printfn $"%A{res}"
        0
