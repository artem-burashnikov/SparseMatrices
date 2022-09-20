namespace spbu_fsharp

open Microsoft.FSharp.Core

module Main =

    // Homework 1 - Task 1.
    // Power function.
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power function.
    let rec pow (arg: float) (exp: int) : float =

        // This operation is not defined
        if arg = 0 && exp = 0 then
            failwith "Undefined"

        // Accumulate the product through a loop.
        let mutable result: float = 1.0

        for i = 1 to abs exp do
            result <- result * arg

        // For a positive exponent return the value as is.
        // For a negative exponent inverse the number.
        if exp > 0 then result else 1.0 / result



    // Homework 1 - Task 2.
    // Quick power function.
    // Ths function takes two numbers (base and exponent)
    // and recursively calculates a power function.
    let rec q_pow (arg: float) (exp: int) : float =

        // This operation is not defined
        if arg = 0 && exp = 0 then
            failwith "Undefined"

        let result: float =

            // Recursion base case.
            if exp = 0 then
                1.0
            else
                // Divide the exponent by half (floor is taken for an odd argument).
                let halve: float = q_pow arg (abs exp / 2)
                // To get an even exponent multiply its halves.
                if exp % 2 = 0 then
                    halve * halve
                else
                    // For an odd exponent additionally multiply by the arg.
                    halve * halve * arg

        // For a positive exponent return the value as is.
        // For a negative exponent inverse the number.
        if exp > 0 then result else 1.0 / result


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
        printfn $"Naive power function result: "
        printfn $"Fast power function result: "
        printfn $"Difference between min and max element in a list: "
        printfn $"All odd numbers in between two integers: "
        0
