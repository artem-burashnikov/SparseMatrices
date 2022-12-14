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
        List.map (fun (i, j) -> abs i % size, abs j % size) tupleLst
        |> List.distinct
        |> List.map func


    /// Returns a list of (value, weight) from a list of values and a given weight.
    let makeCOOTuples intList weight size =
        List.map (fun x -> abs x % size) intList
        |> List.distinct
        |> List.map (fun x -> (x, Some weight))


    /// Returns an Array2D from a given list of triplets (i, j, weight).
    let cooTriplesToTable (tripletsList: List<int * int * 'a>) size =
        let arr = Array2D.create size size Option.None

        for i = 0 to tripletsList.Length - 1 do
            arr[Converter.first tripletsList[i], Converter.second tripletsList[i]] <-
                Converter.third tripletsList[i] |> Some

        arr


    /// Returns an array from a given list of tuples (i, weight).
    let cooTuplesToTable (tuplesList: List<int * 'a>) size =
        let arr = Array.create size Option.None

        for i = 0 to tuplesList.Length - 1 do
            arr[fst tuplesList[i]] <- snd tuplesList[i] |> Some

        arr


    let naiveBFS (startV: List<int>) (mtx: 'a option[,]) =
        let queue = startV |> List.map (fun x -> (x, 0))

        let addToQueue queue vertex iter =
            let rec inner list counter =
                if counter < 0 then
                    list
                else
                    let value = mtx[vertex, counter]

                    if value = Option.None then
                        inner list (counter - 1)
                    else
                        inner (list @ [ counter, iter ]) (counter - 1)

            inner queue (Array2D.length2 mtx - 1)


        let rec inner queue result visited =
            match queue with
            | [] -> result
            | (vertex, iter) :: tl ->
                if List.contains vertex visited then
                    inner tl result visited
                else
                    let visited = vertex :: visited
                    let newQ = addToQueue tl vertex (iter + 1)
                    inner newQ ((vertex, Some iter) :: result) visited

        if queue.IsEmpty then [] else inner queue [] [] |> List.rev


    [<Tests>]
    let tests =

        testList
            "Breadth-First Search"
            [

              testProperty
                  "BinTree from COO converter: Should produce the same tree structure as an array-to-tree converter."
              <| fun (lst: List<int>) (s: int) ->

                  let size = (abs s) + 1

                  let inputList = makeCOOTuples lst 1 size

                  let inputArr = cooTuplesToTable inputList size

                  let actualResult = COOVector(inputList, size) |> Converter.cooVecToTree

                  let expectedResult = vecToTree inputArr

                  Expect.equal actualResult expectedResult ""


              testProperty
                  "QuadTree from COO converter: Should produce the same tree structure as an array2d-to-tree converter."
              <| fun (tupleLst: List<int * int>) (s: int) ->
                  let size = (abs s) + 1

                  let inputList = makeCOOTriplets tupleLst size

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = COOMatrix(inputList, size, size) |> Converter.cooMtxToTree

                  let expectedResult = tableToTree inputTable

                  Expect.equal actualResult expectedResult ""


              testProperty "NaiveBFS and BFS should produce equal results"
              <| fun (ints: List<int>) (tupleLst: List<int * int>) (s: int) ->
                  let size = (abs s) + 1

                  let startV = List.map (fun i -> abs i % size) ints |> List.distinct

                  let inputList = makeCOOTriplets tupleLst size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult =
                      COOVector(naiveBFS startV inputTable, size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: empty starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0, 1); (0, 2); (1, 3); (2, 3) ]

                  let size = 4

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  Expect.equal actualResult BinTree.None ""


              testCase "BFS: All vertices are starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0, 1); (0, 2); (1, 3); (2, 3) ]

                  let size = 4

                  let startV = [ 0; 1; 2; 3 ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult =
                      COOVector([ (0, Some 0); (1, Some 0); (2, Some 0); (3, Some 0) ], size)
                      |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, non-empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0

                  let startV = [ 0; 1; 2; 3 ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (Graphs.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> Converter.cooVecToTree

                  Expect.equal actualResult expectedResult "" ]
