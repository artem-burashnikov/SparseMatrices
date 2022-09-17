namespace spbu_fsharp

open System.Reflection


module Main =


    let rec sum x acc =
        if x = 0 then
            acc
        else
            sum (x - 1) (acc + x)

    [<EntryPoint>]
    let main (argv: string array) =

        printfn $"res: %A{sum 10 0}"
        0
