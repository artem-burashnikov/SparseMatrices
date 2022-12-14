module Task5Tests

open System
open HomeWork4.MatrixData
open HomeWork4.VectorData
open HomeWork5
open Microsoft.FSharp.Collections
open Expecto
open Microsoft.FSharp.Core
open Trees.BinTrees

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

module GeneralFunctions =

    let r = Random()


    /// Generates random weight to a tuple. Returns a triplet.
    let func (i, j) =
        let res = r.Next(10)

        if res % 2 = 0 then
            (i, j, Option.None)
        else
            (i, j, Some res)


    /// Normalizes tuples values according to a given size.
    let makeCOOTriplets tupleLst size =
        List.map (fun (i, j) -> i % size, j % size) tupleLst
        |> List.distinct
        |> List.map func


    /// Returns a list of (value, weight) from a list of values and a given weight.
    let makeCOOTuples uintList weight size =
        List.map (fun x -> x % size) uintList
        |> List.distinct
        |> List.map (fun x -> (x, Some weight))


    /// Returns an Array2D from a given list of triplets (i, j, weight).
    let cooTriplesToTable (tripletsList: List<uint * uint * Option<'a>>) size =
        let s = Helpers.Numbers.toIntConv size
        let arr = Array2D.create s s Option.None

        for i = 0 to tripletsList.Length - 1 do
            let indexI = Converter.first tripletsList[i] |> Helpers.Numbers.toIntConv
            let indexJ = Converter.second tripletsList[i] |> Helpers.Numbers.toIntConv
            arr[indexI, indexJ] <- Converter.third tripletsList[i] |> Some

        arr


    /// Returns an array from a given list of tuples (i, weight).
    let cooTuplesToTable (tuplesList: List<uint * Option<'a>>) size =
        let s = Helpers.Numbers.toIntConv size
        let arr = Array.create s Option.None

        for i = 0 to tuplesList.Length - 1 do
            let index = Helpers.Numbers.toIntConv (fst tuplesList[i])
            arr[index] <- snd tuplesList[i] |> Some

        arr


    let naiveBFS (startV: List<uint>) (mtx: Option<'a>[,]) =
        let queue = startV |> List.map (fun x -> (x, 0u))

        let addToQueue (queue: List<uint * uint>) (vertex: uint) iter =
            let rec inner (list: List<uint * uint>) counter =
                if counter < 0 then
                    list
                else
                    let i = Helpers.Numbers.toIntConv vertex
                    let value = mtx[i, counter]

                    if value = Option.None then
                        inner list (counter - 1)
                    else
                        inner (list @ [ counter |> uint, iter ]) (counter - 1)

            inner queue (Array2D.length2 mtx - 1)


        let rec innerBFS queue result visited =
            match queue with
            | [] -> result
            | (vertex, iter) :: tl ->
                if List.contains vertex visited then
                    innerBFS tl result visited
                else
                    let visited = vertex :: visited
                    let newQ = addToQueue tl vertex (iter + 1u)
                    innerBFS newQ ((vertex, Some iter) :: result) visited

        if queue.IsEmpty then
            []
        else
            innerBFS queue [] [] |> List.rev


    [<Tests>]
    let tests =

        testList
            "Breadth-First Search"
            [

              testProperty
                  "BinTree from COO converter: Should produce the same tree structure as an array-to-tree converter."
              <| fun (lst: List<uint>) (s: uint) ->

                  let size = s + 1u

                  let inputList = makeCOOTuples lst 1 size

                  let inputArr = cooTuplesToTable inputList size

                  let actualResult = COOVector(inputList, size) |> Converter.cooVecToTree

                  let expectedResult = vecToTree inputArr

                  Expect.equal actualResult expectedResult ""


              testProperty
                  "QuadTree from COO converter: Should produce the same tree structure as an array2d-to-tree converter."
              <| fun (tupleLst: List<uint * uint>) (s: uint) ->
                  let size = s + 1u

                  let inputList = makeCOOTriplets tupleLst size

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = COOMatrix(inputList, size, size) |> Converter.cooMtxToTree

                  let expectedResult = tableToTree inputTable

                  Expect.equal actualResult expectedResult ""


              testProperty "NaiveBFS and BFS should produce equal results"
              <| fun (uints: List<uint>) (tupleLst: List<uint * uint>) (s: uint) ->
                  let size = s + 1u

                  let startV = List.map (fun i -> i % size) uints |> List.distinct

                  let inputList = makeCOOTriplets tupleLst size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult =
                      COOVector(naiveBFS startV inputTable, size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: empty starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0u, 1u); (0u, 2u); (1u, 3u); (2u, 3u) ]

                  let size = 4u

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  Expect.equal actualResult BinTree.None ""


              testCase "BFS: All vertices are starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0u, 1u); (0u, 2u); (1u, 3u); (2u, 3u) ]

                  let size = 4u

                  let startV = [ 0u; 1u; 2u; 3u ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult =
                      COOVector([ (0u, Some 0u); (1u, Some 0u); (2u, Some 0u); (3u, Some 0u) ], size)
                      |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, non-empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0u

                  let startV = [ 0u; 1u; 2u; 3u ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0u

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult "" ]
