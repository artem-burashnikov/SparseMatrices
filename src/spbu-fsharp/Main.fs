namespace spbu_fsharp
open HomeWork2

open Lists
open Microsoft.FSharp.Core
module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        printfn $"my sorted list: %A{qSortOOP (MyOOPNonEmptyList(4, MyOOPNonEmptyList(3, MyOOPNonEmptyList(2, MyOOPNonEmptyList(2, MyOOPEmptyList()))))) |> toMyList}"
        0
