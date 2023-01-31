module MatrixReader

open System.IO
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix

type MMObject =
    | Matrix

    static member ObjectFromStr str =
        match str with
        | "matrix" -> Matrix
        | _ -> failwith "Unsupported object type"


type MMFormat =
    | Coordinate

    static member FormatFromStr str =
        match str with
        | "coordinate" -> Coordinate
        | _ -> failwith "Unsupported format type"


type MMField =
    | Real
    | Integer
    | Boolean

    static member FieldFromStr str =
        match str with
        | "real" -> Real
        | "integer" -> Integer
        | "boolean" -> Boolean
        | _ -> failwith "Unsupported field type"


type MMSymmetry =
    | General

    static member SymmetryFromStr str =
        match str with
        | "general" -> General
        | _ -> failwith "Unsupported symmetry type"


type MMFile(filePath: string) =

    let readMetaData (str: string) =
        let metaData = str.Split(" ")

        if metaData.Length < 5 then
            failwith "MatrixReader.MMFile: readMetaData: Incorrect line."
        else
            let object = metaData[1]
            let format = metaData[2]
            let field = metaData[3]
            let symmetry = metaData[4]
            object, format, field, symmetry

    let readSize (str: string) =
        let size = str.Split(" ")

        if size.Length < 3 then
            failwith "MatrixReader.MMFile: readSize: Incorrect line."
        else
            let rows = uint size[0]
            let columns = uint size[1]
            let entries = uint size[2]
            rows, columns, entries

    let allLines =
        if File.Exists filePath then
            File.ReadAllLines filePath
        else
            failwith "MatrixReader.MMFile: No file was found at the specified path."

    // The first line in a file contains metadata.
    let object, format, field, symmetry =
        if allLines.Length > 0 then
            readMetaData (allLines[0].ToLower())
        else
            failwith "MatrixReader.MMFile: The file was empty."

    // Convert to sequence and skip all lines with comments.
    // The first line after the last comment contains parameters of the matrix.
    let sq = Array.toSeq allLines |> Seq.skipWhile (fun line -> line[0] = '%')

    let rows, columns, entries =
        if Seq.length sq > 0 then
            readSize (Seq.head sq)
        else
            failwith "MatrixReader.MMFile: No size information was found inside the file."

    member this.Object = MMObject.ObjectFromStr object
    member this.Format = MMFormat.FormatFromStr format
    member this.Field = MMField.FieldFromStr field
    member this.Symmetry = MMSymmetry.SymmetryFromStr symmetry
    member this.Rows = rows
    member this.Columns = columns
    member this.Entries = entries
    member this.Data = Seq.removeAt 0 sq


type MatrixReader(filePath: string, object: string, field: string) =
    let file = MMFile filePath
    let givenObject = MMObject.ObjectFromStr(object.ToLower())
    let givenField = MMField.FieldFromStr(field.ToLower())

// TODO Everything else.
