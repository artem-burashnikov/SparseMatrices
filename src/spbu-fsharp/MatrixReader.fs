module MatrixReader

open System.IO
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix

let readMatrixToSparse (filePath: string) =

    let lines = File.ReadAllLines filePath

    // The first line contains general information about a given file in a MM format.
    let metaData = lines[0].Split(" ")
    let object = metaData[1]
    let format = metaData[2]
    let field = metaData[3]
    let symmetry = metaData[4]

    // Skip all comments and read matrix parameters and data.
    let mutable dataLineIndex = 1

    while lines[dataLineIndex][0] = '%' do
        dataLineIndex <- dataLineIndex + 1

    let size = lines[dataLineIndex].Split(" ")
    let rows = int size[0]
    let columns = int size[1]

    let rec readDataToList list currLineIndex =
        if currLineIndex > lines.Length - 1 then
            list
        else
            let row, column, value =
                let currentLine = lines[currLineIndex].Split(" ")
                uint currentLine[0], uint currentLine[1], float currentLine[2] |> Some

            readDataToList ((row, column, value) :: list) (currLineIndex + 1)

    // Construct the resulting SparseMatrix.
    COOMatrix(readDataToList [] dataLineIndex, uint rows, uint columns)
    |> SparseMatrix
