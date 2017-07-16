﻿module Utils

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

let keysSet map = map |> Map.toSeq |> Seq.map fst |>Set.ofSeq

type DiffResult<'T> =
| Diff of 'T *'T
| Same of 'T
type OptionDiffResult<'T> =
| OptionDiff of 'T option*'T option
| OptionSame of 'T option
let diff lhs rhs = 
    match lhs=rhs with
    |true -> Same lhs
    |false -> Diff(lhs,rhs)
let optionDiff lhs rhs = 
    match lhs=rhs with
    |true -> OptionSame lhs
    |false -> OptionDiff(lhs,rhs)