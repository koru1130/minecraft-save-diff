namespace MinecraftSaveDiff

module Region =
    open System

    let chunkArrayBySize size arr =
        List.ofArray arr
        |> List.chunkBySize size
        |> List.toArray

    let getChunkLocations (mca:byte[]) =
        mca.[0..4095]
        |> chunkArrayBySize 4
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
        |> chunkArrayBySize 4
        |> Array.map (fun list ->
            BitConverter.ToInt32((Array.rev <| List.toArray list),0)
            )

    let getRawChunkByPos pos (mca:byte[]) =
        if pos > 1023 || pos < 0 then None else
        let (offset,sectorCount) = (getChunkLocations mca).[pos]      
        if offset = 0 then None else
        let length = BitConverter.ToInt32(Array.rev mca.[offset..offset+3],0)
        if length = 0 then None else        
        Some(mca.[offset+4],mca.[offset+5..offset+3+length])

    let getChunkByPos (mca:byte[]) pos =
        getRawChunkByPos pos mca 
        |>Option.map (fun (ct,chunk) -> 
                        match ct with
                          |1uy -> Utils.gzipDecompress chunk
                          |2uy -> Utils.zlibDecompress chunk
                          |_ -> failwith "error zip type"
                          )
                          
    type RegionFile(region:byte[]) =         
        member this.getChunkLocations = getChunkLocations region        
        member this.getChunkByPos pos = getChunkByPos region pos
        member this.getTimeStamps = getTimeStamps region