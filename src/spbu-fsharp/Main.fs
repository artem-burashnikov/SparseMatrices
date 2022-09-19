namespace spbu_fsharp

// open System
open Microsoft.FSharp.Core

module Main =
    exception InvalidInput of string

    // TODO power helper function against unexpected input




    // Homework 1 - Task 1 - Power function - O(n) time complexity
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power function.
    let pow (arg: float) (exp: int) : float =
        let mutable result = 1.0

        for i = 1 to abs exp do
            result <- result * arg

        // For positive exponents return the result.
        // For negative exponents return the inverse.
        if exp > 0 then
            result
        else
            float 1 / result



    // Homework 1 - Task 2 - Quick power function - O(log n) time complexity
    // This function takes two numbers (base and exponent)
    // and recursively calculates a power function.
    let rec q_pow (arg: float) (exp: int) : float =

        let result: float = // The return value

            if exp = 0 then // Recursion base case
                1
            else
                // Divide the exponent by half (floor is taken for an odd argument).
                let halve = q_pow arg (abs exp / 2)

                if exp % 2 = 0 then // To get an even exponent
                    halve * halve // multiply its square roots.
                else
                    halve * halve * arg // For an odd exp additionally multiply by the arg.

        // For positive exponents return the result.
        // For negative exponents return the inverse.
        if exp > 0 then
            result
        else
            float 1 / result


    [<EntryPoint>]
    let main (argv: string array) =

        printfn $"result: {pow 2 5}, quick result: {q_pow 2 5}"

        0
