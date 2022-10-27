module HomeWork4

type Vector<'Value>(arr: 'Value array) =
    // TODO Implement the type. Below is temp.
    member this.Arr = arr

/// Calculate the nearest power of two which is greater than or equal to the given integer.
let ceilPowTwo x =
    let rec looper x acc =
        if acc >= x then
            acc
        else
            looper x (acc * 2)

    // Identity element for multiplication is 1, hence accumulator starts at 1.
    looper x 1
