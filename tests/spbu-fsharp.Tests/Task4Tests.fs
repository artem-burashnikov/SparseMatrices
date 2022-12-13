module Task4Tests

open System
open HomeWork4
open Helpers.Numbers
open HomeWork4.MatrixData
open HomeWork4.VectorData
open Microsoft.FSharp.Collections
open Trees.BinTrees
open Trees.QuadTrees
open Trees
open Expecto
open Microsoft.FSharp.Core

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

module GeneralFunctions =

    [<Tests>]
    let tests =

        testList
            "General functions."
            [

              testCase "ceilPowTwo: Input is equal to 0."
              <| fun _ ->
                  let actualResult = ceilPowTwo 0
                  let expectedResult = 1
                  Expect.equal actualResult expectedResult "Incorrect output."

              testCase "ceilPowTwo: Negative input."
              <| fun _ ->
                  let actualResult = ceilPowTwo -10
                  let expectedResult = 1
                  Expect.equal actualResult expectedResult "Incorrect output."

              testCase "ceilPowTwo: Positive input."
              <| fun _ ->
                  let actualResult = ceilPowTwo 10
                  let expectedResult = 16
                  Expect.equal actualResult expectedResult "Incorrect output."

              testProperty "ceilPowTwo: Output should be no less than a given input."
              <| fun x ->
                  let actualResult = ceilPowTwo x
                  actualResult >= x

              testProperty "ceilPowTwo: Different function that does the same should produce the same result."
              <| fun x ->
                  let result1 = ceilPowTwo x

                  let result2 =
                      if x <= 0 then 1
                      elif x = 1 then 2
                      else pown 2 (int << ceil <| Math.Log2(double x))

                  result1 = result2 ]


module SparseVector =

    [<Tests>]
    let tests =

        testList
            "SparseVector"
            [

              testProperty "Vector partitioning: built-in function should produce the same result."
              <| fun (arr: array<_>) ->
                  let expectedLeft, expectedRight = Array.splitAt ((ceilPowTwo arr.Length) / 2) arr

                  let actualLeft, actualRight = vecDiv2 (Vector(arr, 0, ceilPowTwo arr.Length))

                  Expect.sequenceEqual
                      actualLeft.Memory[actualLeft.Head .. actualLeft.Head + actualLeft.Length - 1]
                      expectedLeft
                      ""

                  Expect.sequenceEqual
                      actualRight.Memory[actualRight.Head .. actualRight.Head + actualRight.Length - 1]
                      expectedRight
                      ""

              testProperty "Partitioning then concatenating should output the initial array"
              <| fun (arr: array<_>) ->
                  let leftPart, rightPart = vecDiv2 (Vector(arr, 0, ceilPowTwo arr.Length))

                  let actualResult =
                      Array.concat
                          [ leftPart.Memory[leftPart.Head .. leftPart.Head + leftPart.Length - 1]
                            rightPart.Memory[rightPart.Head .. rightPart.Head + leftPart.Length - 1] ]

                  Expect.sequenceEqual actualResult arr ""

              testCase "vecToTree: empty array should produce an empty tree"
              <| fun _ ->
                  let actualResult = vecToTree [||]
                  let expectedResult = BinTrees.None
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 1-element array"
              <| fun _ ->
                  let actualResult = vecToTree [| Some 1 |]
                  let expectedResult = BinTrees.Leaf 1
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 2-elements array (different values)"
              <| fun _ ->
                  let actualResult = vecToTree [| Some 1; Some 2 |]

                  let expectedResult = BinTrees.Node(BinTrees.Leaf 1, BinTrees.Leaf 2)

                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 2-elements array (identical values)"
              <| fun _ ->
                  let actualResult = vecToTree [| Some 1; Some 1 |]

                  let expectedResult = BinTrees.Leaf(1)
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 3-elements array (different values)"
              <| fun _ ->
                  let actualResult = vecToTree [| Some 1; Option.None; Some 2 |]

                  let expectedResult =
                      BinTrees.Node(
                          BinTrees.Node(BinTrees.Leaf 1, BinTrees.None),
                          BinTrees.Node(BinTrees.Leaf 2, BinTrees.None)
                      )

                  Expect.equal actualResult expectedResult ""

              testProperty
                  "SparseVector.GetValue loop through each index collecting values. Resulting sequence should be equal to the original."
              <| fun (arr: array<_>) ->
                  let sparseVec = SparseVector.SparseVector arr
                  let mutable actualResult = []

                  for i = 0 to arr.Length - 1 do
                      actualResult <- sparseVec[i] :: actualResult

                  Expect.sequenceEqual (actualResult |> List.rev) arr "" ]


module SparseMatrix =

    let getTableSomeNone rows columns =
        Array2D.init rows columns (fun i j -> Some(i + j))


    [<Tests>]
    let tests =

        testList
            "SparseMatrix"
            [

              testProperty "Table partition: empty input"
              <| fun (arr: int option[,]) ->

                  let mtx = Matrix(arr, 0, 0, 0, 0)

                  let nw, ne, sw, se = mtxDiv4 mtx
                  Expect.equal nw.Rows 0 ""
                  Expect.equal nw.Columns 0 ""
                  Expect.equal ne.Rows 0 ""
                  Expect.equal ne.Columns 0 ""
                  Expect.equal sw.Rows 0 ""
                  Expect.equal sw.Columns 0 ""
                  Expect.equal se.Rows 0 ""
                  Expect.equal se.Columns 0 ""

              testProperty "Table partition (by indices)"
              <| fun (arr: int option[,]) (x: int) (y: int) (rows: int) (columns: int) ->

                  let pX = abs x
                  let pY = abs y
                  let pRows = abs rows
                  let pColumns = abs columns


                  if pRows > 0 && pColumns > 0 then

                      let nX = pX % pColumns
                      let nY = pY % pRows

                      let powerSize = ceilPowTwo (max pRows pColumns)

                      let middle = powerSize / 2

                      // Memory (arr) is not important.
                      let mtx = Matrix(arr, nX, nY, powerSize, powerSize)

                      let nw, ne, sw, se = mtxDiv4 mtx
                      Expect.equal (nw.HeadX, nw.HeadY) (nX, nY) "NW failed"
                      Expect.equal (ne.HeadX, ne.HeadY) (nX, nY + middle) "NE failed"
                      Expect.equal (sw.HeadX, sw.HeadY) (nX + middle, nY) "SW failed"
                      Expect.equal (se.HeadX, se.HeadY) (nX + middle, nY + middle) "SE failed"

              testCase "Table to QuadTree converter: empty table"
              <| fun _ ->
                  let input = Array2D.zeroCreate 0 0
                  let actualResult = tableToTree input
                  let expectedResult = QuadTree.None
                  Expect.equal actualResult expectedResult ""

              testCase "Table to QuadTree converter: 1x0 table"
              <| fun _ ->
                  let input = Array2D.zeroCreate 1 0
                  let actualResult = tableToTree input
                  let expectedResult = QuadTree.None
                  Expect.equal actualResult expectedResult ""

              testCase "Table to QuadTree converter: 0x1 table"
              <| fun _ ->
                  let input = Array2D.zeroCreate 0 1
                  let actualResult = tableToTree input
                  let expectedResult = QuadTree.None
                  Expect.equal actualResult expectedResult ""

              testCase "Table to QuadTree converter: 1x1 table"
              <| fun _ ->
                  let input = Array2D.map Some (Array2D.zeroCreate 1 1)
                  let actualResult = tableToTree input
                  let expectedResult = QuadTree.Leaf 0
                  Expect.equal actualResult expectedResult ""

              testCase "Table to QuadTree converter: 1x2 table"
              <| fun _ ->
                  // let input = Array2D.map Some (Array2D.zeroCreate 1 1)
                  let input = Array2D.init 1 2 (fun i j -> Some(i + j))
                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.None, QuadTree.None)

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 2x1 table"
              <| fun _ ->
                  let input = getTableSomeNone 2 1
                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(QuadTree.Leaf 0, QuadTree.None, QuadTree.Leaf 1, QuadTree.None)

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 2x2 table"
              <| fun _ ->
                  let input = getTableSomeNone 2 2

                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.Leaf 1, QuadTree.Leaf 2)

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 3x2 table"
              <| fun _ ->
                  let input = getTableSomeNone 3 2

                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(
                          QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.Leaf 1, QuadTree.Leaf 2),
                          QuadTree.None,
                          QuadTree.Node(QuadTree.Leaf 2, QuadTree.Leaf 3, QuadTree.None, QuadTree.None),
                          QuadTree.None
                      )

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 3x3 table"
              <| fun _ ->
                  let input = getTableSomeNone 3 3

                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(
                          QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.Leaf 1, QuadTree.Leaf 2),
                          QuadTree.Node(QuadTree.Leaf 2, QuadTree.None, QuadTree.Leaf 3, QuadTree.None),
                          QuadTree.Node(QuadTree.Leaf 2, QuadTree.Leaf 3, QuadTree.None, QuadTree.None),
                          QuadTree.Node(QuadTree.Leaf 4, QuadTree.None, QuadTree.None, QuadTree.None)
                      )

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Getting values from 1x1 table"
              <| fun _ ->

                  let input = Array2D.zeroCreate 1 1

                  let mtx = SparseMatrix.SparseMatrix input

                  for i = 0 to 0 do
                      for j = 0 to 0 do
                          Expect.equal (mtx[i, j]) (input[i, j]) ""

              testCase "Getting values from 1x2 table"
              <| fun _ ->

                  let table = getTableSomeNone 1 2

                  let mtx = SparseMatrix.SparseMatrix table

                  for i = 0 to 0 do
                      for j = 0 to 1 do
                          Expect.equal (mtx[i, j]) (table[i, j]) ""

              testCase "Getting values from 2x1 table"
              <| fun _ ->

                  let table = getTableSomeNone 2 1

                  let mtx = SparseMatrix.SparseMatrix table

                  for i = 0 to 1 do
                      for j = 0 to 0 do
                          Expect.equal (mtx[i, j]) (table[i, j]) ""

              // For some reason this test takes somewhat long.
              testProperty
                  "FsCheck: Getting values from 2d-array and getting values from SparseMatrix should be the same"
              <| fun (arr2d: _ option[,]) ->

                  let rows = Array2D.length1 arr2d
                  let columns = Array2D.length2 arr2d

                  let mtx = SparseMatrix.SparseMatrix arr2d

                  for i = 0 to rows - 1 do
                      for j = 0 to columns - 1 do
                          Expect.equal (mtx[i, j]) (arr2d[i, j]) $"%A{arr2d}, %A{mtx.Data}" ]


module Algebra =

    let r = Random()

    let toSomeNone x =
        if x % 2 = 1 then Some x else Option.None

    let valueToZero x = if x % 2 = 1 then x else 0

    let fromZeroToSomeNone x = if x <> 0 then Some x else Option.None

    let fPlus a b =
        match a, b with
        | Option.None, Option.None -> Option.None
        | Some x, Some y ->
            let result = x + y
            if x = -y then Option.None else Some result
        | Option.None, Some y -> Some y
        | Some x, Option.None -> Some x

    let fMinus a b =
        match a, b with
        | Option.None, Option.None -> Option.None
        | Some x, Some y ->
            let result = x - y
            if x = y then Option.None else Some result
        | Option.None, Some y -> Some(-y)
        | Some x, Option.None -> Some x

    let fMult a b =
        match a, b with
        | Some x, Some y ->
            let result = x * y
            if result = 0 then Option.None else Some result
        | _ -> Option.None


    let naiveMtxMtx (table1: int[,]) (table2: int[,]) =
        let mtx1Rows = Array2D.length1 table1
        let mtx2Rows = Array2D.length1 table2
        let mtx2Columns = Array2D.length2 table2

        let mutable result = Array2D.zeroCreate mtx1Rows mtx2Columns

        for i = 0 to mtx1Rows - 1 do
            for j = 0 to mtx2Columns - 1 do
                for k = 0 to mtx2Rows - 1 do
                    result[i, j] <- result[i, j] + table1[i, k] * table2[k, j]

        result


    let naiveVecByMtx (arr: array<int>) (table: int[,]) =
        let rows = arr.Length
        let columns = Array2D.length2 table
        let mutable result = Array.zeroCreate columns

        for j = 0 to columns - 1 do
            for i = 0 to rows - 1 do
                result[j] <- result[j] + arr[i] * table[i, j]

        result

    let getRandomVector length =
        Array.init length (fun _ -> r.Next(1, 10))

    let getRandomSomeNoneVector length =
        getRandomVector length |> Array.map toSomeNone

    let getRandomTable rows columns =
        Array2D.init rows columns (fun _ _ -> r.Next(1, 10))

    let getRandomSomeNoneTable rows columns =
        getRandomTable rows columns |> Array2D.map toSomeNone


    [<Tests>]
    let tests =

        testList
            "Algebra"
            [

              testProperty "Adding 1 and then subtracting 1 should output the initial data."
              <| fun (length: uint) ->
                  let arr = getRandomSomeNoneVector (int length)

                  let arrOnes = Array.init (int length) (fun _ -> Some 1)
                  let vec = SparseVector.SparseVector arr
                  let vecOnes = SparseVector.SparseVector arrOnes
                  let plusOne = MatrixAlgebra.elementwiseVecVec fPlus vec vecOnes
                  let plusMinusOne = MatrixAlgebra.elementwiseVecVec fMinus plusOne vecOnes

                  Expect.equal
                      plusMinusOne.Data
                      vec.Data
                      $"Array: %A{arr}, vector: %A{vec.Data}, plusOne: %A{plusOne.Data}, plusMinusOne: %A{plusMinusOne.Data}"

              testProperty "Adding/Subtracting 0 should output the initial data."
              <| fun (length: uint) ->
                  let arr = getRandomSomeNoneVector (int length)

                  let zeroes = Array.init (int length) (fun _ -> Option.None)
                  let vec = SparseVector.SparseVector arr
                  let vecZeroes = SparseVector.SparseVector zeroes
                  let plusZero = MatrixAlgebra.elementwiseVecVec fPlus vec vecZeroes
                  let minusZero = MatrixAlgebra.elementwiseVecVec fMinus plusZero vecZeroes
                  Expect.equal minusZero.Data vec.Data ""

              testProperty "Commutative property should hold."
              <| fun (length: uint) ->
                  let arr1 = getRandomSomeNoneVector (int length)

                  let arr2 = getRandomSomeNoneVector (int length)

                  let vec1 = SparseVector.SparseVector arr1
                  let vec2 = SparseVector.SparseVector arr2
                  let result1 = MatrixAlgebra.elementwiseVecVec fPlus vec1 vec2
                  let result2 = MatrixAlgebra.elementwiseVecVec fPlus vec2 vec1
                  Expect.equal result1.Data result2.Data ""

              testProperty "Associative property should hold."
              <| fun (length: uint) ->
                  let arr1 = getRandomSomeNoneVector (int length)

                  let arr2 = getRandomSomeNoneVector (int length)

                  let arr3 = getRandomSomeNoneVector (int length)

                  let vec1 = SparseVector.SparseVector arr1
                  let vec2 = SparseVector.SparseVector arr2
                  let vec3 = SparseVector.SparseVector arr3

                  let result1 =
                      MatrixAlgebra.elementwiseVecVec fPlus (MatrixAlgebra.elementwiseVecVec fPlus vec1 vec2) vec3

                  let result2 =
                      MatrixAlgebra.elementwiseVecVec fPlus (MatrixAlgebra.elementwiseVecVec fPlus vec2 vec3) vec1

                  Expect.equal result1.Data result2.Data ""

              testProperty "Subtracting oneself should result in neutral element."
              <| fun (length: uint) ->
                  let arr = getRandomSomeNoneVector (int length)

                  let vec = SparseVector.SparseVector arr
                  let result = MatrixAlgebra.elementwiseVecVec fMinus vec vec
                  Expect.equal result.Data BinTrees.None ""

              testCase "Vector 1x1 * 1x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1 |]
                  let table = getRandomTable 1 1
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix tableSome

                  let actualResult = (MatrixAlgebra.vecByMtx fPlus fMult vec mtx)

                  let expectedResult = BinTree.Leaf(table[0, 0])

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Array: %A{arr}, vector: %A{vec.Data}, table: %A{table}, QuadTre: %A{mtx.Data}, actualResult: %A{actualResult.Data}"

                  Expect.equal actualResult.Length (Array2D.length2 table) ""

              testCase "Vector 1x2 * 2x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1; Some 1 |]

                  let table = getRandomTable 2 1
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix tableSome

                  let actualResult = MatrixAlgebra.vecByMtx fPlus fMult vec mtx

                  let expectedResult = BinTrees.Leaf(table[0, 0] + table[1, 0]) |> reduceBt

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Input arr: %A{arr}. Input table %A{table}. Vector data: %A{vec.Data}. Matrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length (Array2D.length2 table) ""

              testCase "Vector 1x3 * 3x2 Matrix = Vector 1x2"
              <| fun _ ->
                  let arr = [| Some 1; Some 1; Some 1 |]

                  let table = getRandomTable 3 2
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix tableSome

                  let actualResult = MatrixAlgebra.vecByMtx fPlus fMult vec mtx

                  let expectedResult =
                      BinTree.Node(
                          BinTree.Leaf(table[0, 0] + table[1, 0] + table[2, 0]),
                          BinTree.Leaf(table[0, 1] + table[1, 1] + table[2, 1])
                      )
                      |> reduceBt

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Input arr: %A{arr}. Input table %A{table}. Vector data: %A{vec.Data}. Matrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length (Array2D.length2 table) ""

              testProperty "Vector x Matrix"
              <| fun (x: int) (y: int) ->
                  if x <> 0 && y <> 0 then
                      let length = abs x
                      let rows = length
                      let columns = abs y

                      // Initialize array of numbers and randomly change some of them to zeroes.
                      // After which map Some and None.
                      // We still need unmapped data, so this actions are separate.
                      let arr = getRandomVector length |> Array.map valueToZero
                      let arrSome = Array.map fromZeroToSomeNone arr

                      // Do the same with Array2D.
                      let table = getRandomTable rows columns |> Array2D.map valueToZero

                      let tableSome = table |> Array2D.map fromZeroToSomeNone

                      // Then we make a vector and a matrix using the data and multiply them.
                      let vec = SparseVector.SparseVector arrSome
                      let mtx = SparseMatrix.SparseMatrix tableSome

                      // We also calculate the naive approach of multiplying array and a table.
                      // Result from tree*tree and arr*table should match.
                      let expectedResult =
                          naiveVecByMtx arr table
                          |> Array.map fromZeroToSomeNone
                          |> SparseVector.SparseVector

                      let actualResult = MatrixAlgebra.vecByMtx fPlus fMult vec mtx

                      Expect.equal
                          actualResult.Data
                          expectedResult.Data
                          $"Input array: %A{arrSome}, \nInput table %A{tableSome}, \nVector data: %A{vec.Data}, \nMatrix data: %A{mtx.Data}"

                      Expect.equal actualResult.Length mtx.Columns ""

              testProperty "Vector x Matrix: multiplying by Id from the right."
              <| fun (x: int) ->
                  if x <> 0 then
                      let length = abs x

                      // Initialize array of numbers and randomly change some of them to zeroes.
                      // After which map Some and None.
                      let arr = getRandomVector length |> Array.map valueToZero
                      let arrSome = Array.map fromZeroToSomeNone arr

                      // Initialize Identity matrix for a given vector.
                      let table = Array2D.init length length (fun i j -> if i = j then 1 else 0)

                      let tableSome = table |> Array2D.map fromZeroToSomeNone

                      // Then we make a vector and a matrix using the data and multiply them.
                      let vec = SparseVector.SparseVector arrSome
                      let mtx = SparseMatrix.SparseMatrix tableSome

                      // We also calculate the naive approach of multiplying array and a table.
                      // Result from tree*tree and arr*table should match.
                      let expectedResult =
                          naiveVecByMtx arr table
                          |> Array.map fromZeroToSomeNone
                          |> SparseVector.SparseVector

                      let actualResult = MatrixAlgebra.vecByMtx fPlus fMult vec mtx

                      Expect.equal
                          actualResult.Data
                          expectedResult.Data
                          $"Input array: %A{arrSome}, \nInput table %A{tableSome}, \nVector data: %A{vec.Data}, \nMatrix data: %A{mtx.Data}"

                      Expect.equal actualResult.Length mtx.Columns ""

              testProperty "Chain mult: Vector x Matrix x Matrix."
              <| fun (x: int) (y: int) (z: int) ->
                  if x <> 0 && y <> 0 && z <> 0 then
                      let length = abs x
                      let rows = length
                      let columns = abs y
                      let columns2 = abs z

                      // Initialize array of numbers and randomly change some of them to zeroes.
                      // After which map Some and None.
                      let arr = getRandomVector length |> Array.map valueToZero
                      let arrSome = Array.map fromZeroToSomeNone arr

                      // Initialize two matrices.
                      let table1 = getRandomTable rows columns |> Array2D.map valueToZero
                      let table2 = getRandomTable columns columns2 |> Array2D.map valueToZero

                      let tableSome1 = table1 |> Array2D.map fromZeroToSomeNone
                      let tableSome2 = table2 |> Array2D.map fromZeroToSomeNone

                      // Then we make a vector and a matrix using the data and multiply them.
                      let vec = SparseVector.SparseVector arrSome
                      let mtx1 = SparseMatrix.SparseMatrix tableSome1
                      let mtx2 = SparseMatrix.SparseMatrix tableSome2

                      // We also calculate the naive approach of multiplying array and a table.
                      // Result from tree*tree and arr*table should match.
                      let expectedResult =
                          naiveVecByMtx (naiveVecByMtx arr table1) table2
                          |> Array.map fromZeroToSomeNone
                          |> SparseVector.SparseVector

                      let actualResult =
                          MatrixAlgebra.vecByMtx fPlus fMult (MatrixAlgebra.vecByMtx fPlus fMult vec mtx1) mtx2

                      Expect.equal actualResult.Data expectedResult.Data ""
                      Expect.equal actualResult.Length mtx2.Columns "" ]
