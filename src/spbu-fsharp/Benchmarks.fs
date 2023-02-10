namespace MyBenchmarks

open BenchmarkDotNet.Attributes
open MatrixAlgebra.MatrixAlgebra
open SparseMatrix.SparseMatrix
open SparseVector.SparseVector

module MatrixAlgebraBenchmarks =
    let r = System.Random()

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

    type MyMultBench() =

        [<GlobalSetup>]
        let mtx =
            Array2D.init 8_000 8_000 (fun i j -> if (i + j) % 3 = 0 then Some(r.Next(1, 10)) else Option.None)
            |> SparseMatrix

        let vec =
            Array.init 8_000 (fun i -> if i % 3 = 0 then Some(r.Next(1, 10)) else Option.None)
            |> SparseVector

        [<Benchmark(Baseline = true)>]
        member this.BaseMult() = vecByMtx 0 fPlus fMult vec mtx

        [<Benchmark>]
        member this.Level2Mult() = vecByMtx 2 fPlus fMult vec mtx

        [<Benchmark>]
        member this.Level4Mult() = vecByMtx 4 fPlus fMult vec mtx

    type MyAddBench() =

        [<GlobalSetup>]
        let vec =
            Array.init 8_000_000 (fun i -> if i % 3 = 0 then Some(r.Next(1, 10)) else Option.None)
            |> SparseVector

        [<Benchmark(Baseline = true)>]
        member this.BaseAdd() = SparseVector.Map2 0 fPlus vec vec

        [<Benchmark>]
        member this.Level2Add() = SparseVector.Map2 2 fPlus vec vec

        [<Benchmark>]
        member this.Level4Add() = SparseVector.Map2 4 fPlus vec vec
