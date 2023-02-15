module MatrixReader

open System.IO
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix

/// This type represents Matrix Market objects that MatrixReader is able to read.
type MMObject =
    | Matrix

    static member ObjectFromStr str =
        match str with
        | "matrix" -> Matrix
        | _ -> failwith $"Unsupported object type: {str}"


/// This type represents Matrix Market formats that MatrixReader is able to read.
type MMFormat =
    | Coordinate

    static member FormatFromStr str =
        match str with
        | "coordinate" -> Coordinate
        | _ -> failwith $"Unsupported format type {str}"


/// This type represents Matrix Market fields that MatrixReader is able to read.
type MMField =
    | Real
    | Integer
    | Pattern

    static member FieldFromStr str =
        match str with
        | "real" -> Real
        | "integer" -> Integer
        | "pattern" -> Pattern
        | _ -> failwith $"Unsupported field type: {str}"


/// This type represents Matrix Market symmetry variants that MatrixReader is able to read.
type MMSymmetry =
    | General
    | Symmetric

    static member SymmetryFromStr str =
        match str with
        | "general" -> General
        | "symmetric" -> Symmetric
        | _ -> failwith $"Unsupported symmetry type: {str}"


/// This type is used by MatrixReader to read a header, general parameters
/// and an unformatted data (represented as a sequence of strings) of an MM object from the file.
type MMFile(filePath: string) =

    let readMetaData (str: string) =
        let metaData = str.Split(" ")

        if metaData.Length < 5 then
            failwith "MatrixReader.MMFile: readMetaData: Incorrect line"
        else
            let object = metaData[1]
            let format = metaData[2]
            let field = metaData[3]
            let symmetry = metaData[4]
            object, format, field, symmetry

    let readSize (str: string) =
        let size = str.Split(" ")

        if size.Length < 3 then
            failwith "MatrixReader.MMFile: readSize: Incorrect line"
        else
            let rows = uint size[0]
            let columns = uint size[1]
            let entries = uint size[2]
            rows, columns, entries

    let allLines = File.ReadLines filePath |> Seq.cast<string>

    // The first line in a file contains metadata.
    let object, format, field, symmetry =
        let head = Seq.tryHead allLines

        if head <> None then
            readMetaData (head.Value.ToLower())
        else
            failwith "MatrixReader.MMFile: The file was empty"

    // Skip all sequential lines with comments.
    // The first line after the last comment contains parameters of the matrix.
    let sq = Seq.skipWhile (fun (x: string) -> x[0] = '%') allLines

    let rows, columns, entries =
        let head = Seq.tryHead sq

        if head <> None then
            readSize head.Value
        else
            failwith "MatrixReader.MMFile: No size information was found inside the file"

    member this.Object = MMObject.ObjectFromStr object
    member this.Format = MMFormat.FormatFromStr format
    member this.Field = MMField.FieldFromStr field
    member this.Symmetry = MMSymmetry.SymmetryFromStr symmetry
    member this.Rows = rows
    member this.Columns = columns
    member this.Entries = entries
    member this.Data = Seq.removeAt 0 sq


/// Read Matrix Market file.
type MatrixReader(filePath: string) =

    // Make MMFile object and assert correct data specification.
    let file = MMFile filePath

    do
        if file.Object <> Matrix then
            failwith $"Object specified in a file: %s{(string file.Object).ToLower()} is not supported"

        if file.Format <> Coordinate then
            failwith $"Format specified in a file %s{(string file.Format).ToLower()} is not supported"

        if file.Field <> Integer && file.Field <> Real && file.Field <> Pattern then
            failwith $"Field specified in a file %s{(string file.Field).ToLower()} is not supported"

    // Since symmetric data only contains coordinates in a lower triangle, we need to mirror it to the upper triangle of the matrix.
    /// Function makes a new sequence without (i, i) coordinates, then maps (i,j) to (j,i) and appends this new sequence to the initial.
    let useSymmetry symmetry sq =
        let mirrorBySymmetry sq =
            let mapping triplet =
                let i, j, v = triplet
                (j, i, v)

            Seq.filter (fun (i, j, _) -> i <> j) sq |> Seq.map mapping |> Seq.append sq

        match symmetry with
        | General -> sq |> Seq.toList
        | Symmetric -> mirrorBySymmetry sq |> Seq.toList

    // The following method is used for reading actual data from the file.
    // Indices are offset by -1 since coordinates in the data start at (1,1) and we use (0,0) for vertices.
    // The result is a SparseMatrix.
    member this.Read(converter: string -> 'A) =

        let mapping (str: string) =
            let result = str.Split()
            uint result[0] - 1u, uint result[1] - 1u, converter result[2]

        let data =
            let sq = Seq.map mapping file.Data
            useSymmetry file.Symmetry sq

        COOMatrix(data, file.Rows, file.Columns) |> SparseMatrix
