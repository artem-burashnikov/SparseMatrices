namespace SpbuFsharp

open BenchmarkDotNet.Running
open Benchmarks.MatrixAlgebraBenchmarks

module Main =

    [<EntryPoint>]
    let main (args: string[]) =

        BenchmarkSwitcher.FromTypes([| typeof<BenchSparse> |]).Run(args) |> ignore

        0
