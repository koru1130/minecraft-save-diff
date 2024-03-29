﻿module Region

open System

let getChunkLocations (mca:byte[]) =
    mca.[0..4095]
    |> Utils.chunkArrayBySize 4
    |> Array.map (fun list ->
        let newList = List.rev list
        let offset = BitConverter.ToInt32 ( newList.Tail @ [0uy]
                                           |> List.toArray ,0)*4096                   
        let sectorCount = newList.Head
        (offset,sectorCount) )
    //|> Array.map (fun [a;b;c;d] ->
    //    (BitConverter.ToInt32([|c;b;a;0uy|],0)*4096,d)
    //    )
let getTimeStamps (mca : byte[]) = 
    mca.[4096..8191]
    |> Utils.chunkArrayBySize 4
    |> Array.map (fun list ->
        BitConverter.ToInt32((Array.rev <| List.toArray list),0)
        )

let getRawRawChunkByPos pos (mca:byte[]) =
    if pos > 1023 || pos < 0 then None else
    let (offset,sectorCount) = (getChunkLocations mca).[pos]      
    if offset = 0 then None else
    let length = BitConverter.ToInt32(Array.rev mca.[offset..offset+3],0)
    if length = 0 then None else        
    Some(mca.[offset+4],mca.[offset+5..offset+3+length])
    |>Option.map (fun (ct,chunk) -> 
                    match ct with
                      |1uy -> Utils.gzipDecompress chunk
                      |2uy -> Utils.zlibDecompress chunk
                      |_ -> failwith "error zip type"
                      )

let getRawChunkByPos (mca:byte[]) pos =
    getRawRawChunkByPos pos mca
    |>Option.bind NBT.parse
    |>Option.bind Chunk.tryParse

let getChunkByPos (mca:byte[]) pos =
    getRawChunkByPos mca pos
    |>Option.map Chunk.rawChunk2Chunk
    
type RegionFile(region:byte[]) =         
    member this.getChunkLocations = getChunkLocations region        
    member this.getChunkByPos pos = getChunkByPos region pos
    member this.getTimeStamps = getTimeStamps region