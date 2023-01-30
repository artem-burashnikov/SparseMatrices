module MatrixVectorTests

open System
open Helpers.Numbers
open SparseVector.SparseVector
open SparseMatrix.MatrixData
open SparseVector.VectorData
open SparseMatrix.SparseMatrix
open MatrixAlgebra.MatrixAlgebra
open Microsoft.FSharp.Collections
open Trees.BinTrees
open Trees.QuadTrees
open Trees
open Expecto
open Microsoft.FSharp.Core

let config =
    { FsCheckConfig.defaultConfig with
        maxTest = 10000 }

module GeneralFunctions =

    [<Tests>]
    let tests =

        testList
            "General functions."
            [

              testCase "ceilPowTwo: Input is equal to 0."
              <| fun _ ->
                  let actualResult = ceilPowTwo 0u
                  let expectedResult = 1u
                  Expect.equal actualResult expectedResult "Incorrect output."

              testCase "ceilPowTwo: Positive input."
              <| fun _ ->
                  let actualResult = ceilPowTwo 10u
                  let expectedResult = 16u
                  Expect.equal actualResult expectedResult "Incorrect output."

              testProperty "ceilPowTwo: Output should be no less than a given input."
              <| fun x ->
                  let actualResult = ceilPowTwo x
                  actualResult >= x

              testProperty "ceilPowTwo: Different function that does the same should produce the same result."
              <| fun x ->
                  let result1 = ceilPowTwo x

                  let result2 =
                      if x <= 0u then 1u
                      elif x = 1u then 2u
                      else pown 2u (int << ceil <| Math.Log2(double x))

                  result1 = result2 ]


module SparseVector =

    [<Tests>]
    let tests =

        testList
            "SparseVector"
            [

              testProperty "Vector partitioning: built-in function should produce the same result."
              <| fun (arr: array<_>) ->
                  let powNum = ceilPowTwo (arr.Length |> uint)
                  let expectedLeft, expectedRight = Array.splitAt (powNum / 2u |> toIntConv) arr

                  let actualLeft, actualRight = arrVecPartition (ArrVector(arr, 0u, powNum))

                  let ll, lr = toIntConv actualLeft.Head, toIntConv actualLeft.Length
                  let rl, rr = toIntConv actualRight.Head, toIntConv actualRight.Length

                  Expect.sequenceEqual

                      actualLeft.Memory[ll .. ll + lr - 1]
                      expectedLeft
                      ""

                  Expect.sequenceEqual actualRight.Memory[rl .. rl + rr - 1] expectedRight ""

              testProperty "Partitioning then concatenating should output the initial array"
              <| fun (arr: array<_>) ->
                  let powNum = ceilPowTwo (arr.Length |> uint)
                  let leftPart, rightPart = arrVecPartition (ArrVector(arr, 0u, powNum))

                  let actualResult =
                      let ll, lr = toIntConv leftPart.Head, toIntConv leftPart.Length
                      let rl, rr = toIntConv rightPart.Head, toIntConv rightPart.Length
                      Array.concat [ leftPart.Memory[ll .. ll + lr - 1]; rightPart.Memory[rl .. rl + rr - 1] ]

                  Expect.sequenceEqual actualResult arr ""

              testCase "vecToTree: empty array should produce an empty tree"
              <| fun _ ->
                  let actualResult = arrVecToTree [||]
                  let expectedResult = BinTrees.None
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 1-element array"
              <| fun _ ->
                  let actualResult = arrVecToTree [| Some 1 |]
                  let expectedResult = BinTrees.Leaf 1
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 2-elements array (different values)"
              <| fun _ ->
                  let actualResult = arrVecToTree [| Some 1; Some 2 |]

                  let expectedResult = BinTrees.Node(BinTrees.Leaf 1, BinTrees.Leaf 2)

                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 2-elements array (identical values)"
              <| fun _ ->
                  let actualResult = arrVecToTree [| Some 1; Some 1 |]

                  let expectedResult = BinTrees.Leaf(1)
                  Expect.equal actualResult expectedResult ""

              testCase "vecToTree: 3-elements array (different values)"
              <| fun _ ->
                  let actualResult = arrVecToTree [| Some 1; Option.None; Some 2 |]

                  let expectedResult =
                      BinTrees.Node(
                          BinTrees.Node(BinTrees.Leaf 1, BinTrees.None),
                          BinTrees.Node(BinTrees.Leaf 2, BinTrees.None)
                      )

                  Expect.equal actualResult expectedResult ""

              testProperty
                  "SparseVector.GetValue loop through each index collecting values. Resulting sequence should be equal to the original."
              <| fun (arr: array<_>) ->
                  let sparseVec = SparseVector arr
                  let mutable actualResult = []

                  for i = 0 to arr.Length - 1 do
                      actualResult <- sparseVec[i |> uint] :: actualResult

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

                  let mtx = TableMatrix(arr, 0u, 0u, 0u, 0u)

                  let nw, ne, sw, se = tableMTXPartition mtx
                  Expect.equal nw.Rows 0u ""
                  Expect.equal nw.Columns 0u ""
                  Expect.equal ne.Rows 0u ""
                  Expect.equal ne.Columns 0u ""
                  Expect.equal sw.Rows 0u ""
                  Expect.equal sw.Columns 0u ""
                  Expect.equal se.Rows 0u ""
                  Expect.equal se.Columns 0u ""

              testProperty "Table partition (by indices)"
              <| fun (arr: int option[,]) (x: uint) (y: uint) (rows: uint) (columns: uint) ->

                  let columns = columns + 1u
                  let rows = rows + 1u
                  let x = x % columns
                  let y = y % rows

                  let powerSize = ceilPowTwo (max rows columns)

                  let middle = powerSize / 2u

                  // Memory (arr) is not important.
                  let mtx = TableMatrix(arr, x, y, powerSize, powerSize)

                  let nw, ne, sw, se = tableMTXPartition mtx
                  Expect.equal (nw.HeadX, nw.HeadY) (x, y) "NW failed"
                  Expect.equal (ne.HeadX, ne.HeadY) (x, y + middle) "NE failed"
                  Expect.equal (sw.HeadX, sw.HeadY) (x + middle, y) "SW failed"
                  Expect.equal (se.HeadX, se.HeadY) (x + middle, y + middle) "SE failed"

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

                  let mtx = SparseMatrix input

                  for i = 0 to 0 do
                      for j = 0 to 0 do
                          Expect.equal (mtx[i |> uint, j |> uint]) (input[i, j]) ""

              testCase "Getting values from 1x2 table"
              <| fun _ ->

                  let table = getTableSomeNone 1 2

                  let mtx = SparseMatrix table

                  for i = 0 to 0 do
                      for j = 0 to 1 do
                          Expect.equal (mtx[i |> uint, j |> uint]) (table[i, j]) ""

              testCase "Getting values from 2x1 table"
              <| fun _ ->

                  let table = getTableSomeNone 2 1

                  let mtx = SparseMatrix table

                  for i = 0 to 1 do
                      for j = 0 to 0 do
                          Expect.equal (mtx[i |> uint, j |> uint]) (table[i, j]) ""

              // For some reason this test takes somewhat long.
              testProperty
                  "FsCheck: Getting values from 2d-array and getting values from SparseMatrix should be the same"
              <| fun (arr2d: _ option[,]) ->

                  let rows = Array2D.length1 arr2d
                  let columns = Array2D.length2 arr2d

                  let mtx = SparseMatrix arr2d

                  for i = 0 to rows - 1 do
                      for j = 0 to columns - 1 do
                          Expect.equal (mtx[i |> uint, j |> uint]) (arr2d[i, j]) $"%A{arr2d}, %A{mtx.Data}" ]


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
                  let l = toIntConv length

                  let arr = getRandomSomeNoneVector l

                  let arrOnes = Array.init l (fun _ -> Some 1)
                  let vec = SparseVector arr
                  let vecOnes = SparseVector arrOnes
                  let plusOne = elementwiseVecVec fPlus vec vecOnes
                  let plusMinusOne = elementwiseVecVec fMinus plusOne vecOnes

                  Expect.equal
                      plusMinusOne.Data
                      vec.Data
                      $"Array: %A{arr}, vector: %A{vec.Data}, plusOne: %A{plusOne.Data}, plusMinusOne: %A{plusMinusOne.Data}"

              testProperty "Adding/Subtracting 0 should output the initial data."
              <| fun (length: uint) ->
                  let l = toIntConv length

                  let arr = getRandomSomeNoneVector l

                  let zeroes = Array.init l (fun _ -> Option.None)
                  let vec = SparseVector arr
                  let vecZeroes = SparseVector zeroes
                  let plusZero = elementwiseVecVec fPlus vec vecZeroes
                  let minusZero = elementwiseVecVec fMinus plusZero vecZeroes
                  Expect.equal minusZero.Data vec.Data ""

              testProperty "Commutative property should hold."
              <| fun (length: uint) ->
                  let l = toIntConv length

                  let arr1 = getRandomSomeNoneVector l

                  let arr2 = getRandomSomeNoneVector l

                  let vec1 = SparseVector arr1
                  let vec2 = SparseVector arr2
                  let result1 = elementwiseVecVec fPlus vec1 vec2
                  let result2 = elementwiseVecVec fPlus vec2 vec1
                  Expect.equal result1.Data result2.Data ""

              testProperty "Associative property should hold."
              <| fun (length: uint) ->
                  let l = toIntConv length

                  let arr1 = getRandomSomeNoneVector l

                  let arr2 = getRandomSomeNoneVector l

                  let arr3 = getRandomSomeNoneVector l

                  let vec1 = SparseVector arr1
                  let vec2 = SparseVector arr2
                  let vec3 = SparseVector arr3

                  let result1 = elementwiseVecVec fPlus (elementwiseVecVec fPlus vec1 vec2) vec3

                  let result2 = elementwiseVecVec fPlus (elementwiseVecVec fPlus vec2 vec3) vec1

                  Expect.equal result1.Data result2.Data ""

              testProperty "Subtracting oneself should result in neutral element."
              <| fun (length: uint) ->
                  let l = toIntConv length

                  let arr = getRandomSomeNoneVector l

                  let vec = SparseVector arr
                  let result = elementwiseVecVec fMinus vec vec
                  Expect.equal result.Data BinTrees.None ""

              testCase "Vector 1x1 * 1x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1 |]
                  let table = getRandomTable 1 1
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector arr
                  let mtx = SparseMatrix tableSome

                  let actualResult = (vecByMtx fPlus fMult vec mtx)

                  let expectedResult = BinTree.Leaf(table[0, 0])

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Array: %A{arr}, vector: %A{vec.Data}, table: %A{table}, QuadTre: %A{mtx.Data}, actualResult: %A{actualResult.Data}"

                  Expect.equal actualResult.Length ((Array2D.length2 table) |> uint) ""

              testCase "Vector 1x2 * 2x1 Matrix = Vector 1x1"
              <| fun _ ->
                  let arr = [| Some 1; Some 1 |]

                  let table = getRandomTable 2 1
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector arr
                  let mtx = SparseMatrix tableSome

                  let actualResult = vecByMtx fPlus fMult vec mtx

                  let expectedResult = BinTrees.Leaf(table[0, 0] + table[1, 0]) |> reduce

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Input arr: %A{arr}. Input table %A{table}. Vector data: %A{vec.Data}. Matrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length ((Array2D.length2 table) |> uint) ""

              testCase "Vector 1x3 * 3x2 Matrix = Vector 1x2"
              <| fun _ ->
                  let arr = [| Some 1; Some 1; Some 1 |]

                  let table = getRandomTable 3 2
                  let tableSome = table |> Array2D.map Some

                  let vec = SparseVector arr
                  let mtx = SparseMatrix tableSome

                  let actualResult = vecByMtx fPlus fMult vec mtx

                  let expectedResult =
                      BinTree.Node(
                          BinTree.Leaf(table[0, 0] + table[1, 0] + table[2, 0]),
                          BinTree.Leaf(table[0, 1] + table[1, 1] + table[2, 1])
                      )
                      |> reduce

                  Expect.equal
                      actualResult.Data
                      expectedResult
                      $"Input arr: %A{arr}. Input table %A{table}. Vector data: %A{vec.Data}. Matrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length ((Array2D.length2 table) |> uint) ""

              testProperty "Vector x Matrix"
              <| fun (length: uint) (columns: uint) ->
                  // if x <> 0 && y <> 0 then
                  let rows = length

                  let l = toIntConv length
                  let r = toIntConv rows
                  let c = toIntConv columns

                  // Initialize array of numbers and randomly change some of them to zeroes.
                  // After which map Some and None.
                  // We still need unmapped data, so this actions are separate.
                  let arr = getRandomVector l |> Array.map valueToZero
                  let arrSome = Array.map fromZeroToSomeNone arr

                  // Do the same with Array2D.
                  let table = getRandomTable r c |> Array2D.map valueToZero

                  let tableSome = table |> Array2D.map fromZeroToSomeNone

                  // Then we make a vector and a matrix using the data and multiply them.
                  let vec = SparseVector arrSome
                  let mtx = SparseMatrix tableSome

                  // We also calculate the naive approach of multiplying array and a table.
                  // Result from tree*tree and arr*table should match.
                  let expectedResult =
                      naiveVecByMtx arr table |> Array.map fromZeroToSomeNone |> SparseVector

                  let actualResult = vecByMtx fPlus fMult vec mtx

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      $"Input array: %A{arrSome}, \nInput table %A{tableSome}, \nVector data: %A{vec.Data}, \nMatrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length mtx.Columns ""

              testProperty "Vector x Matrix: multiplying by Id from the right."
              <| fun (length: uint) ->
                  let l = toIntConv length
                  // Initialize array of numbers and randomly change some of them to zeroes.
                  // After which map Some and None.
                  let arr = getRandomVector l |> Array.map valueToZero
                  let arrSome = Array.map fromZeroToSomeNone arr

                  // Initialize Identity matrix for a given vector.
                  let table = Array2D.init l l (fun i j -> if i = j then 1 else 0)

                  let tableSome = table |> Array2D.map fromZeroToSomeNone

                  // Then we make a vector and a matrix using the data and multiply them.
                  let vec = SparseVector arrSome
                  let mtx = SparseMatrix tableSome

                  // We also calculate the naive approach of multiplying array and a table.
                  // Result from tree*tree and arr*table should match.
                  let expectedResult =
                      naiveVecByMtx arr table |> Array.map fromZeroToSomeNone |> SparseVector

                  let actualResult = vecByMtx fPlus fMult vec mtx

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      $"Input array: %A{arrSome}, \nInput table %A{tableSome}, \nVector data: %A{vec.Data}, \nMatrix data: %A{mtx.Data}"

                  Expect.equal actualResult.Length mtx.Columns ""

              testProperty "Chain mult: Vector x Matrix x Matrix."
              <| fun (length: uint) (columns: uint) (columns2: uint) ->
                  let l = toIntConv length
                  let rows = length
                  let r = toIntConv rows
                  let c = toIntConv columns
                  let c2 = toIntConv columns2

                  // Initialize array of numbers and randomly change some of them to zeroes.
                  // After which map Some and None.
                  let arr = getRandomVector l |> Array.map valueToZero
                  let arrSome = Array.map fromZeroToSomeNone arr

                  // Initialize two matrices.
                  let table1 = getRandomTable r c |> Array2D.map valueToZero
                  let table2 = getRandomTable c c2 |> Array2D.map valueToZero

                  let tableSome1 = table1 |> Array2D.map fromZeroToSomeNone
                  let tableSome2 = table2 |> Array2D.map fromZeroToSomeNone

                  // Then we make a vector and a matrix using the data and multiply them.
                  let vec = SparseVector arrSome
                  let mtx1 = SparseMatrix tableSome1
                  let mtx2 = SparseMatrix tableSome2

                  // We also calculate the naive approach of multiplying array and a table.
                  // Result from tree*tree and arr*table should match.
                  let expectedResult =
                      naiveVecByMtx (naiveVecByMtx arr table1) table2
                      |> Array.map fromZeroToSomeNone
                      |> SparseVector

                  let actualResult = vecByMtx fPlus fMult (vecByMtx fPlus fMult vec mtx1) mtx2

                  Expect.equal actualResult.Data expectedResult.Data ""
                  Expect.equal actualResult.Length mtx2.Columns "" ]
