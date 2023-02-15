namespace SpbuFsharp

open BenchmarkDotNet.Running
open Benchmarks.MatrixAlgebraBenchmarks

module Main =

    [<EntryPoint>]
    let main _ =

        let addSummary = BenchmarkRunner.Run<MyAddBench>()
        let multSummary = BenchmarkRunner.Run<MyMultBench>()

        0
