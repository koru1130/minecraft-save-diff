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
