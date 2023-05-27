namespace Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Exporters.Csv
open BreadthFirstSearch.BFS
open Helpers
open SparseMatrix.SparseMatrix
open Graphs
open MatrixReader
open BenchmarkDotNet.Configs

module MatrixAlgebraBenchmarks =

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
            graph <- MatrixReader(fp).Read(Converters.pattern) |> Graph

        [<Benchmark>]
        member this.SparseGraphBFS() = BFS this.ParallelLevel [ 0u ] graph
