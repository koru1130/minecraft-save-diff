namespace MinecraftSaveDiff

module Region =
    open System

    let chunkArrayBySize size arr =
        List.ofArray arr
        |> List.chunkBySize size
        |> List.toArray

    let getChunkLocations (mca : byte[]) =
        mca.[0..4095]
        |> chunkArrayBySize 4
        |> Array.map (fun [a;b;c;d] ->       
            printfn "%A" (BitConverter.ToUInt32([|c;b;a;0uy|],0))
            (BitConverter.ToInt32([|c;b;a;0uy|],0)*4096,d)
            )

    let getChunkByPos pos (mca:byte[]) =        
        let (offset,sectorCount) = (getChunkLocations mca).[pos]      
        if offset = 0 then None else
        let length = BitConverter.ToInt32(Array.rev mca.[offset..offset+3],0)
        if length = 0 then None else        
        Some(mca.[offset+4],mca.[offset+5..offset+3+length])

    type RegionFile(region:byte[]) =         
        member this.getChunkLocations = getChunkLocations region        
        member this.getChunkByPos pos = getChunkByPos pos region