namespace MinecraftSaveDiff

open System.IO

module Utils =
        
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
