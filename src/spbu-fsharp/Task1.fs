module HomeWork1

// Homework 1 - Task 1.
// Power function.
// This function takes two numbers (base and exponent)
// and iteratively calculates a power of base to the exponent.
let pow (arg: float) (exp: int) : float =

    // This operation is not defined
    if arg = 0.0 && exp = 0 then
        failwith "Undefined"

    // Accumulate the product through a loop.
    let mutable result = 1.0

    for i = 1 to abs exp do
        result <- result * arg

    // For a positive exponent return the value as is.
    // For a negative exponent inverse the number.
    if exp > 0 then result else 1.0 / result



// Homework 1 - Task 2.
// Quick power function.
// Ths function takes two numbers (base and exponent)
// and recursively calculates a power of base to the exponent.
let rec qPow (arg: float) (exp: int) =

    // This operation is not defined.
    if arg = 0.0 && exp = 0 then
        failwith "undefined"

    let result =

        // Recursion base case.
        if exp = 0 then
            1.0
        elif exp = 1 then
            arg
        else
            // Divide the exponent by half (floor is taken for an odd argument).
            let halve = qPow arg (abs exp / 2)
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
// The function takes an array of real numbers and returns the difference
// between the maximum and the minimum values.
let diff (arr: float array) : float =

    let result = // This will be returned.

        if Array.isEmpty arr then
            failwith "This operation with an empty array is not supported."
        else

            let mutable mx = arr[0]
            let mutable mn = arr[0]

            // Look at each element in an array from left to right
            // and iteratively find min and max values.
            for i = 1 to arr.Length - 1 do

                // Max on the current iteration.
                if arr[i] > mx then
                    mx <- arr[i]

                // Min on the current iteration.
                if arr[i] < mn then
                    mn <- arr[i]

            mx - mn // The desired value

    result



// Homework 1 - Task 4.
// This function returns the array of all odd integers
// strictly in between two given integers.
let allOdds (num1: int) (num2: int) : int array =

    // Determine the range
    let smallerNum: int = if num1 <= num2 then num1 else num2

    let biggerNum: int = if num1 <= num2 then num2 else num1

    // Make an array of all odd integers in the specified range
    let result: int array =
        [| for i in (smallerNum + 1) .. (biggerNum - 1) do
               if abs i % 2 = 1 then
                   yield i |]

    result // Return the array.
