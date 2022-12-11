module Task5Tests

open System
open HomeWork4
open Helpers.Numbers
open HomeWork4.MatrixData
open HomeWork4.SparseVector
open HomeWork4.VectorData
open HomeWork5
open Microsoft.FSharp.Collections
open Trees.BinTrees
open Trees.QuadTrees
open Trees
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

              testProperty "BinTree from COO converter"
              <| fun (lst: int list) (l: int) ->
                  // We initialize length + 1 because we want to avoid dividing by modulo zero.
                  let length = (abs l) + 1
                  // We normalize coordinates, so they are within the specified size and leave only distinct values.
                  let inputList = List.map (fun value -> abs value % length) lst |> List.distinct

                  // We treat list of coordinates as a COO vector.
                  // This function converts COO data to corresponding array of Some/None.
                  let func lst (array: bool array) =
                      let rec maker lst =
                          match lst with
                          | [] -> array
                          | hd :: tl ->
                              array[hd] <- true
                              maker tl

                      maker lst


                  let inputArr =
                      func inputList (Array.create length false)
                      |> Array.map (fun x -> if x then Some x else Option.None)

                  let vecFromCoo = COOVector(inputList, length)
                  let actualResult = HomeWork5.Converter.cooVecToTree vecFromCoo
                  let expectedResult = vecToTree inputArr
                  Expect.equal actualResult expectedResult ""

              testProperty "QuadTree from COO converter"
              <| fun (tupleLst: (int * int) list) (s: int) ->
                  let size = (abs s) + 1

                  let func (i, j) =
                      let res = r.Next(10)

                      if res % 2 = 0 then
                          (i, j, Option.None)
                      else
                          (i, j, res |> Some)

                  let inputList =
                      List.map (fun (i, j) -> abs i % size, abs j % size) tupleLst
                      |> List.distinct
                      |> List.map func

                  let func tupleLst (arr: 'a option[,]) =
                      let rec maker lst =
                          match lst with
                          | [] -> arr
                          | (i, j, value) :: tl ->
                              arr[i, j] <- Some value
                              maker tl

                      maker tupleLst

                  let inputArr = func inputList (Array2D.create size size Option.None)
                  let quadFromCoo = COOMatrix(inputList, size, size)
                  let actualResult = HomeWork5.Converter.cooMtxToTree quadFromCoo
                  let expectedResult = tableToTree inputArr
                  Expect.equal actualResult expectedResult "" ]
