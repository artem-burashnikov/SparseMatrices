namespace spbu_fsharp

open System
open HomeWork5
open Microsoft.FSharp.Core
open HomeWork5


module Main =

    type Array2D<'Value> = 'Value[,]

    [<EntryPoint>]
    let main (argv: string array) =



        // let lst =
        //     [ (4, 3, 3)
        //       (1, 3, 2)
        //       (1, 4, 2)
        //       (5, 7, 5)
        //       (9, 9, 9) ]
        //

        //
        //
        // let rows = 16
        // let columns = 16
        // let input = COOMatrix(lst, columns, columns)
        // let res = Converter.cooToTree input
        // printfn $"%A{res}"
        let (x: Array2D<int>) = array2D ([ [ 10 ] ])
        0
