module SparseMatrices.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Exporters.Csv
open SparseMatrices.BFS
open SparseMatrices.Helpers
open SparseMatrices.SparseMatrix
open SparseMatrices.Graphs
open SparseMatrices.MatrixReader
open BenchmarkDotNet.Configs

type MyConfig() as this =
    inherit ManualConfig()
    do this.Add(CsvMeasurementsExporter.Default)

[<Config(typeof<MyConfig>)>]
type BenchSparse() =

    // Path to a .mtx file
    let fp = ""
    let arr2d = array2D [||]
    let mutable graph = SparseMatrix(arr2d) |> Graph

    [<Params(0u, 1u, 2u, 3u, 4u)>]
    member val ParallelLevel = 0u with get, set

    [<GlobalSetup>]
    member this.InitGraph() =
        graph <- MatrixReader(fp).Read(pattern) |> Graph

    [<Benchmark>]
    member this.SparseGraphBFS() = BFS this.ParallelLevel [ 0u ] graph
