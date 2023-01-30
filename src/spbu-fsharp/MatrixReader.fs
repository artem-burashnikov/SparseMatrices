module MatrixReader

open System.IO
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix

let readMatrixToSparse (filePath: string) =

    // Read the first line which contains general information about a given file in a MM format.
    let reader = new StreamReader(filePath)

    let header = reader.ReadLine().Split(" ")
    let object = header[1]
    let format = header[2]
    let field = header[3]
    let symmetry = header[4]

    // Skip comments section.
    while reader.Peek() = int '%' do
        reader.ReadLine() |> ignore

    // Read matrix parameters.
    let size = reader.ReadLine().Split(" ") |> Array.map uint

    let rows = size[0]
    let columns = size[1]
    let entries = size[2]

    // Read matrix data.
    let rec readData lst =
        if reader.Peek() >= 0 then
            let line = reader.ReadLine().Split(" ")
            readData ((uint line[0], uint line[1], float line[2] |> Some) :: lst)
        else
            lst

    // Construct the result.
    COOMatrix(readData [], rows, columns) |> SparseMatrix
