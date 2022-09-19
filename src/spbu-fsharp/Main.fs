namespace spbu_fsharp

open System
open Microsoft.FSharp.Core

module Main =

    // TODO helper function against unexpected input



    // Homework 1 - Task 1.
    // Power function.
    // O(n) time complexity.
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power function.
    let pow (arg: float) (exp: int) : float =

        let result: float = // This will be returned from the function.

            if exp = 0 then
                1.0 // Special case.
            else
                // Accumulates the product using a loop.
                let mutable acc: float = 1.0

                for i = 1 to abs exp do
                    acc <- acc * arg

                // For a positive exponent return the value as is.
                // For a negative exponent inverse the number.
                if exp > 0 then acc else float 1 / acc

        result




    // Homework 1 - Task 2.
    // Quick power function.
    // O(log n) time complexity.
    // This function takes two numbers (base and exponent)
    // and recursively calculates a power function.
    let rec q_pow (arg: float) (exp: int) : float =

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
        if exp > 0 then
            result
        else
            float 1 / result


    [<EntryPoint>]
    let main (argv: string array) =
        printfn $"pow {pow 2 0}"
        // printfn $"%A{argv[0]}"
        // printfn "env.cmdline: %A" <| Environment.GetCommandLineArgs()
        0
