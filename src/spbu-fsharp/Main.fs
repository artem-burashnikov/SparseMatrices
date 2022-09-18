namespace spbu_fsharp

open Microsoft.FSharp.Core

module Main =

    // TODO exponent for negative values

    // Homework 1 - Task 1
    // This function takes two numbers and
    // iteratively rises the first to the power of the second.
    let pow (arg: float) (exp: int) : float =
        let mutable result = 1.0

        for i = 1 to exp do
            result <- result * arg

        result

    // Homework 1 - Task 2
    // This function takes two numbers and uses a more sophisticated
    // approach of rising the first to the power of the second.
    let rec q_pow (arg: float) (exp: int) : float =
        if exp = 0 then
            1
        elif exp = 1 then // Recursion base case
            arg
        else
            // Divide the exponent by half (floor is taken for an odd argument).
            let halve = q_pow arg (exp / 2)

            if exp % 2 = 0 then // To get an even exponent
                halve * halve // multiply its square roots.
            else
                halve * halve * arg // For an odd power additionally multiply by the arg.



    [<EntryPoint>]
    let main (argv: string array) =

        printfn $"{q_pow 2 11}, {pow 2 11}, {pow 2 -10}, {q_pow 2 -10}"

        0
