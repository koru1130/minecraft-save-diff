module Utils

open System.IO
        
let streamToArray (stream:Stream) =
    let outputStream = new MemoryStream()
    stream.CopyTo(outputStream)
    outputStream.ToArray()

let zlibDecompress (byteArray:byte[]) =
    new Compression.DeflateStream(new MemoryStream(byteArray.[2..]),Compression.CompressionMode.Decompress)
    |>streamToArray

let gzipDecompress (byteArray:byte[]) =
    new Compression.GZipStream(new MemoryStream(byteArray),Compression.CompressionMode.Decompress)
    |>streamToArray

let chunkArrayBySize size arr =
    List.ofArray arr
    |> List.chunkBySize size
    |> List.toArray

let byte2Nibbles byte =
    let nibble1 = byte &&& 0x0Fuy
    let nibble2 = byte &&& 0xF0uy >>> 4
    [|nibble1;nibble2|]

let bytes2Nibbles bytes =
    Array.collect byte2Nibbles bytes

let inline keysSet map = map |> Map.toSeq |> Seq.map fst |>Set.ofSeq

type DiffResult<'T,'U> =
| Diff of 'U
| Add of 'T
| Del of 'T
| Same of 'T
| NN 

let getDiffResultFstValue = 
    function
    | Diff (l,r) -> l
    | Add x -> x
    | Del x -> x
    | Same x -> x
    | NN -> failwith "can't get value from NN"
let getDiffResultSndValue = 
    function
    | Diff (l,r) -> r
    | Add x -> x
    | Del x -> x
    | Same x -> x
    | NN -> failwith "can't get value from NN"

let inline diff lhs rhs = 
    match lhs=rhs with
    |true -> Same lhs
    |false -> Diff(lhs,rhs)

let diffOption lhs rhs = 
    match (lhs,rhs) with
    |(Some l,Some r) when l=r -> Same l
    |(Some l,Some r) -> Diff(l,r)
    |(Some l,None) -> Del l
    |(None,Some r) -> Add r
    |(None,None) -> NN

let divRem a b =
    let rem = a%b
    let div = (a-rem)/b
    (div,rem)

let inline (!~) (l:Lazy<'T>) = l.Value

let inline lazyDiff lhs rhs =
    lazy
        diff lhs rhs

let readLines filePath = System.IO.File.ReadLines(filePath)

let (|Line|Lines|) (str:string) =
     match str.Contains("\n") with
     | false -> Line str
     | true -> Lines (List.ofArray (str.Split([|'\n'|])))

type Tree<'Key,'Value> =
|Leaf of 'Value 
|Branch of 'Key * Tree<'Key,'Value> list

let rec formatStringTree prefix tree : seq<string> =
    seq{
        match tree with
        |Leaf x -> 
            if (x<>"") then yield prefix + x
        |Branch (key,trees) ->
            if (key<>"") then yield prefix + key
            yield!
                Seq.ofList trees
                |>Seq.collect (formatStringTree ("  "+prefix))
    }