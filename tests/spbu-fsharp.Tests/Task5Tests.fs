module Task5Tests

open HomeWork4.MatrixData
open HomeWork4.VectorData
open HomeWork5
open Microsoft.FSharp.Collections
open Expecto
open Microsoft.FSharp.Core

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

module GeneralFunctions =

    let r = System.Random()

    [<Tests>]
    let tests =

        testList
            "General functions."
            [

              testProperty
                  "BinTree from COO converter: Should produce the same tree structure as an array-to-tree converter."
              <| fun (lst: int list) (l: int) ->
                  // We initialize length + 1 because we want to avoid dividing by modulo zero.
                  let length = (abs l) + 1
                  // Coordinates are normalized, so they are within the specified size.
                  // Make sure only distinct coordinates are left.
                  let inputList =
                      List.map (fun x -> abs x % length) lst
                      |> List.distinct
                      |> List.map (fun x -> (x, true))

                  // We treat list of coordinates as a COO vector.
                  // This function converts COO data to corresponding array.
                  let listToArr lst (array: bool array) =
                      let rec maker lst =
                          match lst with
                          | [] -> array
                          | (i, _) :: tl ->
                              array[i] <- true
                              maker tl

                      maker lst

                  // Some/None is mapped for further conversion to a tree.
                  let inputArr =
                      listToArr inputList (Array.create length false)
                      |> Array.map (fun x -> if x then Some x else Option.None)

                  let actualResult = COOVector(inputList, length) |> Converter.cooVecToTree
                  let expectedResult = vecToTree inputArr
                  Expect.equal actualResult expectedResult ""


              testProperty
                  "QuadTree from COO converter: Should produce the same tree structure as an array2d-to-tree converter."
              <| fun (tupleLst: (int * int) list) (s: int) ->
                  let size = (abs s) + 1

                  let func (i, j) =
                      let res = r.Next(10)

                      if res % 2 = 0 then
                          (i, j, Option.None)
                      else
                          (i, j, Some res)

                  let inputList =
                      List.map (fun (i, j) -> abs i % size, abs j % size) tupleLst
                      |> List.distinct
                      |> List.map func

                  let listToArr tripletsList (arr: 'a option[,]) =
                      let rec maker lst =
                          match lst with
                          | [] -> arr
                          | (i, j, value) :: tl ->
                              arr[i, j] <- Some value
                              maker tl

                      maker tripletsList

                  let actualResult = COOMatrix(inputList, size, size) |> Converter.cooMtxToTree

                  let expectedResult =
                      listToArr inputList (Array2D.create size size Option.None) |> tableToTree

                  Expect.equal actualResult expectedResult "" ]
