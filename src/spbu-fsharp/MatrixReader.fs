module MatrixReader

open System.IO
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix

let readMatrixToSparse (filePath: string) =

    let lines = File.ReadAllLines filePath

    // The first line which contains general information about a given file in a MM format.
    let header = lines[0].Split(" ")
    let object = header[1]
    let format = header[2]
    let field = header[3]
    let symmetry = header[4]

    // Calculate at which line the data starts, then read matrix parameters and corresponding data.
    let mutable dataLineIndex = 1

    while lines[dataLineIndex][0] = '%' do
        dataLineIndex <- dataLineIndex + 1

    let size = lines[dataLineIndex].Split(" ") |> Array.map int
    let rows = size[0]
    let columns = size[1]
    let entries = size[2]

    let maxDataIndex = entries + dataLineIndex

    let rec readDataToList list currLineIndex =
        if currLineIndex > maxDataIndex then
            list
        else
            let row, column, value =
                let currentLine = lines[currLineIndex].Split(" ")
                uint currentLine[0], uint currentLine[1], float currentLine[2] |> Some

            readDataToList ((row, column, value) :: list) (currLineIndex + 1)

    COOMatrix(readDataToList [] dataLineIndex, uint rows, uint columns)
    |> SparseMatrix
