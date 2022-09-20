namespace spbu_fsharp

// open System
open Microsoft.FSharp.Core

module Main =

    exception InputError of string

    // Homework 1 - Task 1.
    // Power function.
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power function.
    let pow (arg: float) (exp: int) : float =

        if arg = 0 && exp = 0 then // This operation is not defined.
            failwith "Incorrect input"
        elif arg = 0 then // Special case: base = 0, exp <> 0.
            0.0
        elif arg = 1 then // Special case: base = 1, exp <> 0.
            1.0
        elif exp = 0 then // Special case: base > 0, exp = 0.
            1.0
        else
            // Accumulates the product using a loop.
            let mutable acc: float = 1.0

            for i = 1 to abs exp do
                acc <- acc * arg

            // For a positive exponent return the value as is.
            // For a negative exponent inverse the number.
            if exp > 0 then acc else float 1 / acc



    // Homework 1 - Task 2.
    // Quick power function.
    // Ths function takes two numbers (base and exponent)
    // and recursively calculates a power function.
    let rec q_pow (arg: float) (exp: int) : float =

        if arg = 0 && exp = 0 then
            failwith "Incorrect input" // This operation is not defined.
        elif arg = 0 then // Special case: base = 0, exp <> 0.
            0.0
        elif arg = 1 then // Special case: base = 1, exp <> 0.
            1.0
        else
            let result: float = // This will be returned from the function.

                if exp = 0 then // Recursion base case.
                    1.0
                else
                    // Divide the exponent by half (floor is taken for an odd argument).
                    let halve: float = q_pow arg (abs exp / 2)

                    if exp % 2 = 0 then // To get an even exponent
                        halve * halve // multiply its square roots.
                    else
                        halve * halve * arg // For an odd exp additionally multiply by the arg.

            // For positive exponents return the result.
            // For negative exponents return the inverse.
            // This part somehow executes at the end, even though this func is recursive???
            if exp > 0 then
                result
            else
                float 1 / result



    // Homework 1 - Task 3.
    // Difference between the min and the max value in an array.
    // The function takes a list of real numbers and calculates the difference
    // between the maximum and the minimum values.
    let diff (arr: float list) : float = 0



    // Homework 1 - Task 4.
    // List all odd integers in between two given integers.
    let all_odds (num1: int) (num2: int) : int =
        let smaller_num = if num1 <= num2 then num1 else num2

        let bigger_num = if num1 <= num2 then num2 else num1

        0




    [<EntryPoint>]
    let main (argv: string array) =
        printfn $"Naive power function result: {pow 2 2}"
        printfn $"Fast power function result: {q_pow 0 0}"

        printfn
            $"Naive power function result: {diff [ 5
                                                   4
                                                   10
                                                   3
                                                   0
                                                   -11
                                                   5.5
                                                   -10.25 ]}"

        0
