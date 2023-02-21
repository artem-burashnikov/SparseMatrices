namespace Benchmarks

open BenchmarkDotNet.Attributes
open SparseVector.SparseVector
open SparseMatrix.SparseMatrix
open MatrixAlgebra.MatrixAlgebra
open Helpers.Initializers

module MatrixAlgebraBenchmarks =

    let fPlus a b =
        match a, b with
        | Option.None, Option.None -> Option.None
        | Some x, Some y ->
            let result = x + y
            if x = -y then Option.None else Some result
        | Option.None, Some y -> Some y
        | Some x, Option.None -> Some x

    let fMult a b =
        match a, b with
        | Some x, Some y ->
            let result = x * y
            if result = 0 then Option.None else Some result
        | _ -> Option.None

    type MyAddBench() =

        let mutable vec = SparseVector([| Some 0 |])

        [<Params(5_000_000, 10_000_000)>]
        member val Length = 0 with get, set

        [<Params(1u, 2u, 3u, 4u)>]
        member val ParallelLevel = 0u with get, set

        [<Params(10, 50, 90)>]
        member val Density = 0 with get, set

        [<GlobalSetup>]
        member this.SetVector() =
            vec <- SparseVector(initArrayWithDensity this.Density this.Length)

        [<Benchmark(Baseline = true)>]
        member this.BaseAdd() = SparseVector.Map2 0u fPlus vec vec

        [<Benchmark>]
        member this.ParallelAdd() =
            SparseVector.Map2 this.ParallelLevel fPlus vec vec

    type MyMultBench() =

        let mutable vec = SparseVector([| Some 0 |])
        let mutable mtx = SparseMatrix(array2D [| [| Some 0 |] |])

        [<Params(1_000, 3_000, 5_000)>]
        member val Rows = 0 with get, set

        [<Params(1_000, 3_000, 5_000)>]
        member val Columns = 0 with get, set

        [<Params(1u, 2u, 3u, 4u)>]
        member val ParallelLevel = 0u with get, set

        [<Params(10, 50, 90)>]
        member val Density = 0 with get, set

        [<GlobalSetup>]
        member this.SetUpVectorAndMatrix() =
            vec <- SparseVector(initArrayWithDensity this.Density this.Rows)
            mtx <- SparseMatrix(init2DArrayWithDensity this.Density this.Rows this.Columns)

        [<Benchmark(Baseline = true)>]
        member this.BaseMult() = vecByMtx 0u fPlus fMult vec mtx

        [<Benchmark>]
        member this.ParallelMult() =
            vecByMtx this.ParallelLevel fPlus fMult vec mtx
