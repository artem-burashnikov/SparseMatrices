namespace MyBenchmarks

open BenchmarkDotNet.Attributes
open MatrixAlgebra.MatrixAlgebra

module MatrixAlgebraBenchmarks =

    type MyBench() =

        let fPlus a b =
            match a, b with
            | Option.None, Option.None -> Option.None
            | Some x, Some y ->
                let result = x + y
                if x = -y then Option.None else Some result
            | Option.None, Some y -> Some y
            | Some x, Option.None -> Some x

        let vec =
            Array.init 10000000 (fun i -> if i % 2 = 0 then Some(i + 1) else Option.None)
            |> SparseVector.SparseVector.SparseVector

        [<Benchmark(Baseline = true)>]
        member this.NormalAlgebra() = vectorMap2 0 fPlus vec vec

        [<Benchmark>]
        member this.ParallelAlgebra() = vectorMap2 3 fPlus vec vec
