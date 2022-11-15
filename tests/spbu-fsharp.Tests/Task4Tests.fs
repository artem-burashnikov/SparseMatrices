module Task4Tests

open System
open Helpers.Numbers
open HomeWork4
open HomeWork4.MatrixData
open HomeWork4.VectorData
open Trees
open Trees.QuadTrees
open Expecto
open FsCheck
open Microsoft.FSharp.Core


module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "samples"
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

                  result1 = result2

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

                  for i = 1 to arr.Length do
                      actualResult <- sparseVec[i] :: actualResult

                  Expect.sequenceEqual (actualResult |> List.rev) arr ""

              testProperty "Table partition: empty input"
              <| fun (arr: _ option[,]) ->

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
              <| fun (arr: _ option[,]) (x: int) (y: int) (rows: int) (columns: int) ->

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
                  let input = Array2D.init 2 1 (fun i j -> Some(i + j))
                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(QuadTree.Leaf 0, QuadTree.None, QuadTree.Leaf 1, QuadTree.None)

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 2x2 table"
              <| fun _ ->
                  let input = Array2D.init 2 2 (fun i j -> Some(i + j))

                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.Leaf 1, QuadTree.Leaf 2)

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Table to QuadTree converter: 3x2 table"
              <| fun _ ->
                  let input = Array2D.init 3 2 (fun i j -> Some(i + j))

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
                  let input = Array2D.init 3 3 (fun i j -> Some(i + j))

                  let actualResult = tableToTree input

                  let expectedResult =
                      QuadTree.Node(
                          QuadTree.Node(QuadTree.Leaf 0, QuadTree.Leaf 1, QuadTree.Leaf 1, QuadTree.Leaf 2),
                          QuadTree.Node(QuadTree.Leaf 2, QuadTree.None, QuadTree.Leaf 3, QuadTree.None),
                          QuadTree.Node(QuadTree.Leaf 2, QuadTree.Leaf 3, QuadTree.None, QuadTree.None),
                          QuadTree.Node(QuadTree.Leaf 4, QuadTree.None, QuadTree.None, QuadTree.None)
                      )

                  Expect.equal actualResult expectedResult $"Failed to construct from table: %A{input}"

              testCase "Getting values from 1x1 array"
              <| fun _ ->

                  let input = Array2D.zeroCreate 1 1

                  let mtx = SparseMatrix.SparseMatrix input

                  for i = 1 to 1 do
                      for j = 1 to 1 do
                          Expect.equal (mtx[i, j]) (input[i - 1, j - 1]) ""

              testCase "Getting values from 1x2 array"
              <| fun _ ->

                  let table = Array2D.init 1 2 (fun i j -> Some(i + j))

                  let mtx = SparseMatrix.SparseMatrix table

                  for i = 1 to 1 do
                      for j = 1 to 2 do
                          Expect.equal (mtx[i, j]) (table[i - 1, j - 1]) ""

              testCase "Getting values from 2x1 array"
              <| fun _ ->

                  let table = Array2D.init 2 1 (fun i j -> Some(i + j))

                  let mtx = SparseMatrix.SparseMatrix table

                  for i = 1 to 2 do
                      for j = 1 to 1 do
                          Expect.equal (mtx[i, j]) (table[i - 1, j - 1]) ""

              // For some reason this test takes somewhat long.
              testProperty
                  "FsCheck: Getting values from 2d-array and getting values from SparseMatrix should be the same"
              <| fun (arr2d: _ option[,]) ->

                  let rows = Array2D.length1 arr2d
                  let columns = Array2D.length2 arr2d

                  let mtx = SparseMatrix.SparseMatrix arr2d

                  for i = 1 to rows do
                      for j = 1 to columns do
                          Expect.equal (mtx[i, j]) (arr2d[i - 1, j - 1]) $"%A{arr2d}, %A{mtx.Data}"

              testProperty "Adding 1 and then subtracting 1 should output the initial data."
              <| fun (length: uint) ->
                  let arr = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let arrOnes = Array.init (int length) (fun _ -> Some 1)
                  let vec = SparseVector.SparseVector arr
                  let vecOnes = SparseVector.SparseVector arrOnes
                  let plusResult = MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec vecOnes
                  let result = MatrixAlgebra.vecPlusVec (0) (0) (0) (-) plusResult vecOnes
                  Expect.equal vec.Data result.Data ""

              testProperty "Adding/Subtracting 0 should output the initial data."
              <| fun (length: uint) ->
                  let arr = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let zeroes = Array.zeroCreate (int length)
                  let vec = SparseVector.SparseVector arr
                  let vecZeroes = SparseVector.SparseVector zeroes
                  let plusResult = MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec vecZeroes
                  let result = MatrixAlgebra.vecPlusVec (0) (0) (0) (-) plusResult vecZeroes
                  Expect.equal vec.Data result.Data ""

              testProperty "Commutative property should hold."
              <| fun (length: uint) ->
                  let arr = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let arr2 = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let vec1 = SparseVector.SparseVector arr
                  let vec2 = SparseVector.SparseVector arr2
                  let result1 = MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec1 vec2
                  let result2 = MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec2 vec1
                  Expect.equal result1.Data result2.Data ""

              testProperty "Associative property should hold."
              <| fun (length: uint) ->
                  let arr = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let arr2 = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let arr3 = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let vec1 = SparseVector.SparseVector arr
                  let vec2 = SparseVector.SparseVector arr2
                  let vec3 = SparseVector.SparseVector arr3

                  let result1 =
                      MatrixAlgebra.vecPlusVec (0) (0) (0) (+) (MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec1 vec2) vec3

                  let result2 =
                      MatrixAlgebra.vecPlusVec (0) (0) (0) (+) (MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec2 vec3) vec1

                  Expect.equal result1.Data result2.Data ""

              testProperty "Adding the inverse should result in neutral element."
              <| fun (length: uint) ->
                  let arr = Array.init (int length) (fun _ -> Some(Random().Next(1, 10)))
                  let vec = SparseVector.SparseVector arr
                  let result = MatrixAlgebra.vecPlusVec (0) (0) (0) (-) vec vec
                  Expect.equal result.Data BinTrees.None ""
              (*
              testCase "Vector 1x1 * 1x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1 |]

                  let table = Array2D.init 1 1 (fun _ _ -> Some 1)

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix table

                  let actualResult = MatrixAlgebra.vecByMtx (+) (*) vec mtx

                  let expectedResult = BinTrees.BinTree.Leaf 2

                  Expect.equal actualResult.Data expectedResult ""
                  Expect.equal actualResult.Length 1 ""


              testCase "Vector 1x2 * 2x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1; Some 1 |]

                  let table = Array2D.init 2 1 (fun i j -> Some(i + j))

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix table

                  let actualResult = MatrixAlgebra.vecByMtx (+) (*) vec mtx

                  let expectedResult = BinTrees.Node(BinTrees.Leaf 1, BinTrees.None)

                  Expect.equal actualResult.Data expectedResult $"Input arr: %A{arr}. Input table %A{table}. Vector data: %A{vec.Data}. Matrix data: %A{mtx.Data} "
                  Expect.equal actualResult.Length 1 ""

              testCase "Vector 1x3 * 3x2 Matrix = Vector 1x2"
              <| fun _ ->
                  let arr = [| Some 1; Some 1; Some 1 |]

                  let table = Array2D.init 3 2 (fun i j -> Some(i + j))

                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix table

                  let actualResult = MatrixAlgebra.vecByMtx (+) (*) vec mtx

                  let expectedResult =
                      BinTrees.Node(BinTrees.Node(BinTrees.Leaf 3, BinTrees.Leaf 6), BinTrees.None)

                  Expect.equal actualResult.Data expectedResult ""
                  Expect.equal actualResult.Length 2 ""

              testCase "Vector 1x8 * 8x4 Matrix = Vector 1x4"
              <| fun _ ->
                  let arr = [| Some 1; Some 1; Some 1; Some 1; Some 1; Some 1; Some 1; Some 1 |]

                  let table = Array2D.init 8 4 (fun i j -> Some(i + j))
                  let vec = SparseVector.SparseVector arr
                  let mtx = SparseMatrix.SparseMatrix table

                  let actualResult = MatrixAlgebra.vecByMtx (+) (*) vec mtx

                  let expectedResult =
                      BinTrees.Node(
                          BinTrees.Node(
                              BinTrees.Node(BinTrees.Leaf 28, BinTrees.Leaf 36),
                              BinTrees.Node(BinTrees.Leaf 44, BinTrees.Leaf 52)
                          ),
                          BinTrees.None
                      )

                  Expect.equal actualResult.Data expectedResult ""
                  Expect.equal actualResult.Length 4 ""

              testCase "Matrix 3x5 * 5x3 Matrix * 3x3 Matrix = Matrix 3x3"
              <| fun _ ->
                  let arr = [| Some 1; Some 1; Some 1; Some 1; Some 1; Some 1; Some 1; Some 1 |]

                  let table3x5 = Array2D.init 3 5 (fun i j -> Some(1))
                  let table5x3 = Array2D.init 5 3 (fun i j -> Some(1))
                  let table3x3 = Array2D.init 3 3 (fun i j -> Some(1))
                  let vec = SparseVector.SparseVector arr
                  let mtx1 = SparseMatrix.SparseMatrix table3x5
                  let mtx2 = SparseMatrix.SparseMatrix table5x3
                  let mtx3 = SparseMatrix.SparseMatrix table3x3

                  let actualResult = MatrixAlgebra.mtxByMtx (+) (*) (MatrixAlgebra.mtxByMtx (+) (*) mtx1 mtx2) mtx3

                  let expectedResult = QuadTree.None

                  Expect.equal actualResult.Data expectedResult ""
                  Expect.equal actualResult.Rows 3 ""
                  Expect.equal actualResult.Columns 3 "" ]
*)
              ]
