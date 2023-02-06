namespace SpbuFsharp

open BenchmarkDotNet.Running
open Microsoft.FSharp.Core
open MyBenchmarks.MatrixAlgebraBenchmarks


module Main =

    [<EntryPoint>]
    let main _ =

        let summary = BenchmarkRunner.Run<MyBench>()

        0
