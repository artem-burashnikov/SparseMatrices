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
            elif exp = 1 then
                arg
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
    let diff (arr: float array) : float =

        let mutable mx = arr[0]
        let mutable mn = arr[0]

        // Look at each element in an array from left to right
        // and iteratively find min and max values.
        for i = 1 to arr.Length - 1 do

            // Max on the current iteration.
            if arr[i] > mx then
                mx <- arr[i]
            else
                mx <- mx

            // Min on the current iteration.
            if arr[i] < mn then
                mn <- arr[i]
            else
                mn <- mn

        mx - mn // Return the value.



    // Homework 1 - Task 4.
    // This function returns the array of all odd integers
    // strictly in between two given integers.
    let all_odds (num1: int) (num2: int) : int array =

        // Determine the range
        let smaller_num: int = if num1 <= num2 then num1 else num2

        let bigger_num: int = if num1 <= num2 then num2 else num1

        // Make an array of all odd integers in the specified range
        let result: int array =
            [| for i in (smaller_num + 1) .. (bigger_num - 1) do
                   if abs i % 2 = 1 then yield i |]

        result // Return the array.



    [<EntryPoint>]
    printfn $"Power function {pow 2 5}"

    printfn $"Quick power function {q_pow -2 9}"
    printfn $"Difference between max and min: %A{diff [| 1; 2; 7; 8; 9; -10 |]}"
    printf $"Array of odds: %A{all_odds 1 10}"
