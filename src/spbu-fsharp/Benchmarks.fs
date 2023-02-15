namespace Benchmarks

open BenchmarkDotNet.Attributes
open SparseVector.SparseVector
open SparseMatrix.SparseMatrix
open MatrixAlgebra.MatrixAlgebra

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

    // initArrayWithDensity and init2DArrayWithDensity accept density parameter which values have to range from 0 to 100
    // Won't work otherwise
    let initArrayWithDensity (density: int) length =
        let arr = Array.create length Option.None

        for i in 0 .. length - 1 do
            let cellDensity = (float (i + 1) / float length) * 100.0

            if cellDensity <= density then
                arr[i] <- Some(i + 1)

        arr

    let init2DArrayWithDensity (density: int) rows columns =
        let table = Array2D.create rows columns Option.None

        for i in 0 .. rows - 1 do
            for j in 0 .. columns - 1 do
                let cellDensity = (float (i * columns + j + 1) / float (rows * columns)) * 100.0

                if cellDensity <= density then
                    table[i, j] <- Some(i + j + 1)

        table

    type MyAddBench() =

        let mutable vec = SparseVector([| Some 0 |])

        [<Params(100_000, 1_000_000, 3_000_000, 5_000_000, 7_000_000)>]
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

        [<Params(100_000, 1_000_000, 3_000_000, 5_000_000, 7_000_000)>]
        member val Length = 0 with get, set

        [<Params(1u, 2u, 3u, 4u)>]
        member val ParallelLevel = 0u with get, set

        [<Params(10, 50, 90)>]
        member val Density = 0 with get, set

        [<GlobalSetup>]
        member this.SetUpVectorAndMatrix() =
            vec <- SparseVector(initArrayWithDensity this.Density this.Length)
            mtx <- SparseMatrix(init2DArrayWithDensity this.Density this.Length this.Length)

        [<Benchmark(Baseline = true)>]
        member this.BaseMult() = vecByMtx 0u fPlus fMult vec mtx

        [<Benchmark>]
        member this.ParallelMult() =
            vecByMtx this.ParallelLevel fPlus fMult vec mtx
