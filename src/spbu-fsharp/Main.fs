namespace spbu_fsharp

open System
open System.Drawing.Imaging
open Microsoft.FSharp.Core

module Main =


    // Homework 1 - Task 1.
    // Power function.
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power function.
    let pow (arg: float) (exp: int) : float =

        if exp = 0 then // Special case.
            1.0
        else
            // Accumulates the product using a loop.
            let mutable acc: float = 1.0

            for i = 1 to abs exp do
                acc <- acc * arg

            // For a positive exponent return the value as is.
            // For a negative exponent inverse the number.
            if exp > 0 then acc else float 1 / acc



    // TODO remake this
    // Homework 1 - Task 2.
    // Quick power function.
    // Ths function takes two numbers (base and exponent)
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

    // Homework 1 - Task 3.
    // Difference between the min and the max value in an array.
    // The function takes a list of real numbers and calculates the value
    // using built-in libraries.
    let diff (arr: float list) : float =
        let result =
            if List.length arr > 0 then
                List.max arr - List.min arr
            else
                failwith "Bad input!"

        result



    // Homework 1 - Task 4.
    // List all odd integers in between two given integers.
    // TODO

    [<EntryPoint>]
    let main (argv: string array) =

        // printfn $"%A{argv[0]}"
        // printfn "env.cmdline: %A" <| Environment.GetCommandLineArgs()
        0
