namespace spbu_fsharp.Tests

open Expecto
open spbu_fsharp.Main

module EqualityTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            // Power function.
            [ testCase "1.0 pow. Positive integer to the power of positive exponent"
              <| fun _ ->
                  let actualResult = pow 2 10
                  Expect.equal actualResult 1024 "Results don't match"

              testCase "1.1: pow. Positive integer to the power of negative exponent"
              <| fun _ ->
                  let actualResult = pow 2 -2
                  Expect.equal actualResult 0.25 "Results don't match"

              testCase "1.2: pow. Negative integer to the power of even positive exponent"
              <| fun _ ->
                  let actualResult = pow -2 10
                  Expect.equal actualResult 1024 "Results don't match"

              testCase "1.3: pow. Negative integer to the power of odd positive exponent"
              <| fun _ ->
                  let actualResult = pow -2 9
                  Expect.equal actualResult -512 "Results don't match"

              testCase "1.4: pow. Negative integer to the power of even negative exponent"
              <| fun _ ->
                  let actualResult = pow -2 -2
                  Expect.equal actualResult 0.25 "Results don't match"

              testCase "1.5: pow. Negative integer to the power of odd negative exponent"
              <| fun _ ->
                  let actualResult = pow -2 -1
                  Expect.equal actualResult -0.5 "Results don't match"

              // I have no idea why it works.
              testCase "1.6: pow. 0 to the 0th power is not defined"
              <| fun _ ->
                  let actualResult = Expect.throws (fun _ -> pow 0 0 |> ignore) "undefined"
                  actualResult

              // Fast power function.
              testCase "2.0: qPow. Positive integer to the power of negative exponent"
              <| fun _ ->
                  let actualResult = pow 2 -2
                  Expect.equal actualResult 0.25 "Results don't match"

              testCase "2.1: qPow. Negative integer to the power of even positive exponent"
              <| fun _ ->
                  let actualResult = pow -2 10
                  Expect.equal actualResult 1024 "Results don't match"

              testCase "2.2: qPow. Negative integer to the power of odd positive exponent"
              <| fun _ ->
                  let actualResult = pow -2 9
                  Expect.equal actualResult -512 "Results don't match"

              testCase "2.3: qPow. Negative integer to the power of even negative exponent"
              <| fun _ ->
                  let actualResult = pow -2 -2
                  Expect.equal actualResult 0.25 "Results don't match"

              testCase "2.4: qPow. Negative integer to the power of odd negative exponent"
              <| fun _ ->
                  let actualResult = pow -2 -1
                  Expect.equal actualResult -0.5 "Results don't match"

              testCase "2.5: pow. 0 to the 0th power is not defined"
              <| fun _ ->
                  let actualResult = Expect.throws (fun _ -> qPow 0 0 |> ignore) "undefined"
                  actualResult

              // Min-Max diff.
              testCase "3.1: diff. All numbers are the same"
              <| fun _ ->
                  let actualResult = diff [| 0; 0; 0; 0; 0 |]
                  Expect.equal actualResult 0 "Results don't match"

              testCase "3.2: diff. All numbers are negative."
              <| fun _ ->
                  let actualResult = diff [| -1; -2; -3; -4; -5 |]

                  Expect.equal actualResult 4 "Results don't match"

              testCase "3.3: diff. All numbers are positive."
              <| fun _ ->
                  let actualResult = diff [| 1; 2; 3; 4; 5 |]
                  Expect.equal actualResult 4 "Results don't match"

              testCase "3.4: diff. Positive and negative numbers."
              <| fun _ ->
                  let actualResult = diff [| 1; 2; -3; -4; -5 |]

                  Expect.equal actualResult 7 "Results don't match"

              testCase "3.5: diff. An array only has one element"
              <| fun _ ->
                  let actualResult = diff [| 5 |]
                  Expect.equal actualResult 0 "Results don't match"

              testCase "3.6: diff. An empty array is given"
              <| fun _ ->
                  let actualResult = Expect.throws (fun _ -> diff [||] |> ignore) "undefined"
                  actualResult

              // An array of all odds.
              testCase "4.1: all_ods. In between two positive integers"
              <| fun _ ->
                  let actualResult = allOdds 1 10
                  Expect.equal actualResult [| 3; 5; 7; 9 |] "Results don't match"

              testCase "4.2: all_ods. In between two negative integers"
              <| fun _ ->
                  let actualResult = allOdds -10 -1
                  Expect.equal actualResult [| -9; -7; -5; -3 |] "Results don't match"

              testCase "4.3: all_ods. In between a positive and a negative integer"
              <| fun _ ->
                  let actualResult = allOdds -5 5
                  Expect.equal actualResult [| -3; -1; 1; 3 |] "Results don't match"

              testCase "4.4: all_ods. The same number is given twice"
              <| fun _ ->
                  let actualResult = allOdds 5 5
                  Expect.equal actualResult [||] "Results don't match" ]
