module Task5Tests

open System
open System.Collections.Generic
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
            let indexI =
                Helpers.GeneralFunction.takeFirst tripletsList[i] |> Helpers.Numbers.toIntConv

            let indexJ =
                Helpers.GeneralFunction.takeSecond tripletsList[i] |> Helpers.Numbers.toIntConv

            arr[indexI, indexJ] <- Helpers.GeneralFunction.takeThird tripletsList[i] |> Some

        arr


    /// Returns an array from a given list of tuples (i, weight).
    let cooTuplesToTable (tuplesList: List<uint * Option<'a>>) size =
        let s = Helpers.Numbers.toIntConv size
        let arr = Array.create s Option.None

        for i = 0 to tuplesList.Length - 1 do
            let index = Helpers.Numbers.toIntConv (fst tuplesList[i])
            arr[index] <- snd tuplesList[i] |> Some

        arr

    /// Naive BFS approach.
    let naiveBFS (startV: List<uint>) (mtx: Option<'a>[,]) =

        let length = Array2D.length2 mtx

        // Enqueue starting vertices.
        let queue = Queue<uint * uint>()

        for x in startV do
            queue.Enqueue(x, 0u)

        /// Function adds successors of a given vertex to the queue.
        let addToQueue vertexIndex iter =
            for j = 0 to length - 1 do
                let i = Helpers.Numbers.toIntConv vertexIndex
                let value = mtx[i, j]

                if value <> Option.None then
                    queue.Enqueue(j |> uint, iter)

        /// Go through the queue accumulating the result.
        let rec innerBFS result visited =
            if queue.Count = 0 then
                result
            else
                // Dequeue an item.
                // If it was already visited then skip it,
                // otherwise mark the vertex as visited, go through its neighbours and add them to the queue.
                // After which accumulate the vertex to the result.
                let vertex, iter = queue.Dequeue()

                if Set.contains vertex visited then
                    innerBFS result visited
                else
                    // let visited = vertex :: visited
                    addToQueue vertex (iter + 1u)
                    innerBFS ((vertex, iter) :: result) (Set.add vertex visited)

        // If the starting queue is empty then return immediately, otherwise traverse the graph.
        if queue.Count = 0 then [] else innerBFS [] Set.empty


    [<Tests>]
    let tests =

        testList
            "Breadth-First Search"
            [

              testProperty
                  "BinTree from COO Converters: Should produce the same tree structure as an array-to-tree Converters."
              <| fun (lst: List<uint>) (s: uint) ->

                  let size = s + 1u

                  let inputList = makeCOOTuples lst 1 size

                  let inputArr = cooTuplesToTable inputList size

                  let actualResult = COOVector(inputList, size) |> cooVecToTree

                  let expectedResult = arrVecToTree inputArr

                  Expect.equal actualResult expectedResult ""


              testProperty
                  "QuadTree from COO Converters: Should produce the same tree structure as an array2d-to-tree Converters."
              <| fun (tupleLst: List<uint * uint>) (s: uint) ->
                  let size = s + 1u

                  let inputList = makeCOOTriplets tupleLst size

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = COOMatrix(inputList, size, size) |> cooMtxToTree

                  let expectedResult = tableToTree inputTable

                  Expect.equal actualResult expectedResult ""


              testProperty "NaiveBFS and BFS should produce equal results"
              <| fun (uints: List<uint>) (tupleLst: List<uint * uint>) (s: uint) ->
                  let size = s + 1u

                  let startV = List.map (fun i -> i % size) uints |> List.distinct

                  let inputList = makeCOOTriplets tupleLst size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let inputTable = cooTriplesToTable inputList size

                  let actualResult = (BreadthFirstSearch.BFS startV cooMtx).Data

                  let expectedResult = COOVector(naiveBFS startV inputTable, size) |> cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: empty starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0u, 1u); (0u, 2u); (1u, 3u); (2u, 3u) ]

                  let size = 4u

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (BreadthFirstSearch.BFS startV cooMtx).Data

                  Expect.equal actualResult BinTree.None ""


              testCase "BFS: All vertices are starting vertices"
              <| fun _ ->

                  let tuplesList = [ (0u, 1u); (0u, 2u); (1u, 3u); (2u, 3u) ]

                  let size = 4u

                  let startV = [ 0u; 1u; 2u; 3u ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (BreadthFirstSearch.BFS startV cooMtx).Data

                  let expectedResult =
                      COOVector([ (0u, 0u); (1u, 0u); (2u, 0u); (3u, 0u) ], size) |> cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, non-empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0u

                  let startV = [ 0u; 1u; 2u; 3u ]

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (BreadthFirstSearch.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> cooVecToTree

                  Expect.equal actualResult expectedResult ""


              testCase "BFS: Empty graph, empty start should return empty path."
              <| fun _ ->

                  let tuplesList = []

                  let size = 0u

                  let startV = []

                  let inputList = makeCOOTriplets tuplesList size

                  let cooMtx = COOMatrix(inputList, size, size)

                  let actualResult = (BreadthFirstSearch.BFS startV cooMtx).Data

                  let expectedResult = COOVector([], size) |> cooVecToTree

                  Expect.equal actualResult expectedResult "" ]
