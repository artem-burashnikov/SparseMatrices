namespace SpbuFsharp

open BenchmarkDotNet.Running
open MyBenchmarks.MatrixAlgebraBenchmarks

module Main =

    [<EntryPoint>]
    let main _ =

        let summary1 = BenchmarkRunner.Run<MyAddBench>()
        let summary2 = BenchmarkRunner.Run<MyMultBench>()

        0
