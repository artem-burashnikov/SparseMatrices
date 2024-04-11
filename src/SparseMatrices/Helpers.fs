module SparseMatrices.Helpers

open Microsoft.FSharp.Collections
open System

/// Function calculates the smallest power of two which is greater than or equal to the given integer.
let ceilPowTwo x =

    let rec looper x acc =
        if acc >= x then acc else looper x (acc * 2u)

    if x = 0u then 1u
    elif x = 1u then 2u
    else looper x 1u

/// Function calculates the exponent needed to get the smallest power of two which is greater than or equal to the given integer.
let powTwo x =

    let rec looper x acc power =
        if acc >= x then power else looper x (acc * 2u) (power + 1u)

    if x = 1u then 0u else looper x 1u 0u

let toIntConv (unsignedInt: uint) =
    try
        Convert.ToInt32(unsignedInt)
    with :? OverflowException ->
        failwith $"%A{unsignedInt} is outside the range of the Int32 type."

let parseFloat (input: string) =
    try
        float input
    with _ ->
        failwith $"parseFloat: Invalid input: {input}"

let parseInt (input: string) =
    try
        int input
    with _ ->
        failwith $"parseInt: Invalid input: {input}"

let takeFirst (a, _, _) = a
let takeSecond (_, a, _) = a
let takeThird (_, _, a) = a

// initArrayWithDensity and init2DArrayWithDensity accept density parameter which values have to range from 0 to 100
// Won't work otherwise
let initArrayWithDensity (density: int) length =
    let arr = Array.create length Option.None

    for i in 0 .. length - 1 do
        let cellDensity = (float (i + 1) / float length) * 100.0

        if cellDensity <= density then
            arr[i] <- Some(i + 1)

    arr

let init2DArrayWithDensity (density: int) rows columns =
    let table = Array2D.create rows columns Option.None

    for i in 0 .. rows - 1 do
        for j in 0 .. columns - 1 do
            let cellDensity = (float (i * columns + j + 1) / float (rows * columns)) * 100.0

            if cellDensity <= density then
                table[i, j] <- Some(i + j + 1)

    table

let real (arr: array<string>) = float arr[2]

let integer (arr: array<string>) = int arr[2]

let pattern _ = ()
