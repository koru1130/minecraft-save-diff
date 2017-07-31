namespace MinecraftSaveDiff
open System.IO
open Utils
open BlackFox.ColoredPrintf.ColoredPrintf

module main=

    let duration f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        returnValue 
    [<EntryPoint>]
    let main argv = 
        //let level = new FileStream("level.dat",FileMode.Open)
        //let unziped = new Compression.GZipStream(level,Compression.CompressionMode.Decompress)
        //let reader = new NBT.NBTReader(unziped)
        //printfn "%A" <| reader.readTag()                

        let mca1 = File.ReadAllBytes(argv.[0])
        let mca2 = File.ReadAllBytes(argv.[1])
        let read pos mca = Region.getChunkByPos mca pos 
        //let readNBT pos mca = 
        //    Region.getRawChunkByPos mca pos
        //    |>Option.bind NBT.parse
      //  let getBlocks mca pos = 
      //      read pos mca
      //      |>Option.map Chunk.getBlocks 
       (* let f() = 
            
         //   let diff mca1 mca2 pos =
         //       //let getBlocks = (read pos) >> (Option.map Chunk.getBlocks)
         //       match  ((getBlocks mca1 pos),(getBlocks mca2 pos)) with
         //       |(Some l , Some r) -> Some <| (Chunk.diffBlocks l r)
         //       |(_,_) -> None
         //   
            //diff mca1 mca2 416            
            //let diff = Chunk.diffChunk
                
            //    match  ((Chunk.getBlocks c1),(Chunk.getBlocks c2)) with
            //    |(l,r) -> Chunk.diffBlocks l r
            //    |(_,_) -> None
            
            let output cord diffResult =                
                match !~diffResult with
                |Diff (l,r)-> printfn "%s: %A -> %A" cord l r
                |Add x-> printfn "%s: Add %A" cord x
                |Del x-> printfn "%s: Del %A" cord x
                | _ -> ()
            
            let formatCord (cord:Chunk.Cord) =
                sprintf "%3i %2i %3i" cord.x cord.y cord.z
            
            let formatBlock = 
                
                fun blockID data ->
                    

            let diff l r = 
                Chunk.diffChunkSections l r            
                |>Seq.fold
                    (fun state diffResult ->
                        let (diff,add,del,same) = state
                        match diffResult with
                        |Diff x -> ((List.ofArray x) @ diff,add,del,same)
                        |Add x -> (diff,x::add,del,same)
                        |Del x -> (diff,add,x::del,same)
                        |Same x -> (diff,add,del,x::same)
                        |NN -> failwith "WTF"

                    ) ([],[],[],[])
                |>fun (diff,add,del,same) -> (Map.ofList diff,add,del,same)    
            

            [|0..1023|]
            |>Array.Parallel.map 
                (fun x ->
                    printfn "%i" x
                    let chunk1 = read x mca1
                    let chunk2 = read x mca2
                    match (chunk1,chunk2) with
                    |(Some l,Some r) -> 
                        match l.LastUpdate = r.LastUpdate with
                        |true -> None
                        |false ->Some (diff l r)
                    |(_,_) -> None
                )
            |>Array.map 
                (fun x-> 
                    match x with
                    | Some (x,_,_,_) ->  
                        Map.iter
                            (fun cord (diffResult:Chunk.BlockDiffResultRecord) ->                                                                
                                
                                //printfn "%A" json
                                //let cord = formatCord cord
                                //output cord diffResult.blockID
                                //output cord diffResult.data
                                //output cord diffResult.blockLight                                
                                //output cord diffResult.skyLight
                                //match !~diffResult.dataTag with
                                //| NBT.NBTDiffResult.Same x -> ()
                                //| x -> printfn "%s: NBT" cord 

                                
                            ) x
                    | None -> ()
                ) *)
            //|>printfn "%A"
            //|>Array.Parallel.map (fun pos -> getBlocks mca1 pos)
            
            //let diff rhs lhs

            (*
            read mca1 416
            |>Option.map 
                (fun chunk ->
                    Chunk.getBlocks chunk
                    |>Map.map
                        (fun cord block ->
                            
                        )
                )

           *)
        let f () =            
            let idName =
                readLines "items.tsv"
                |>Seq.map
                    (fun line ->
                        let [|id;data;name;testId|] = line.Split [|'\t'|]
                        ((int id,byte data),(name,testId))
                    )
                |>Map.ofSeq
                
            let (|FormatCord|) (cord:Chunk.Cord) =
                sprintf "%3i %2i %3i" cord.x cord.y cord.z
            
            let getBlockName blockID data =
                match Map.tryFind (blockID,data) idName with
                |Some x -> fst x                    
                |None ->
                    match Map.tryFind (blockID,0uy) idName with
                    |Some x -> sprintf "%s:%i" (fst x) data
                    |None -> sprintf "%i:%i" blockID data
        
            let diff l r = 
                Chunk.diffChunkSections l r            
                |>Seq.fold
                    (fun state diffResult ->
                        let (diff,add,del,same) = state
                        match diffResult with
                        |Diff x -> ((List.ofArray x) @ diff,add,del,same)
                        |Add x -> (diff,x::add,del,same)
                        |Del x -> (diff,add,x::del,same)
                        |Same x -> (diff,add,del,x::same)
                        |NN -> failwith "WTF"
        
                    ) ([],[],[],[])
                |>fun (diff,add,del,same) -> (Map.ofList diff,add,del,same)    
                                   
            [|0..1023|]
            |>Array.Parallel.map 
                (fun x ->
                    //printfn "%i" x
                    let chunk1 = read x mca1
                    let chunk2 = read x mca2
                    match (chunk1,chunk2) with
                    |(Some l,Some r) -> 
                        match l.LastUpdate = r.LastUpdate with
                        |true -> None
                        |false ->Some (diff l r)
                    |(_,_) -> None
                )
            |>Array.map 
                (fun x->
                    match x with
                    | Some (x,_,_,_) ->  
                        Map.iter
                            (fun (FormatCord cord) (diffResult:Chunk.BlockDiffResultRecord) ->                                                                
                                match !~diffResult.blockID with
                                |Diff (l,r)->
                                    colorprintfn "$yellow[%s]:  $red[%s] $yellow[->] $green[%s]"
                                        cord
                                        (getBlockName l (getDiffResultFstValue !~diffResult.data))
                                        (getBlockName r (getDiffResultSndValue !~diffResult.data))
                                    true
                                |Add x->                                    
                                    colorprintfn "$yellow[%s]: $green[+%s]" cord (getBlockName x (getDiffResultSndValue !~diffResult.data))
                                    true
                                |Del x-> 
                                    colorprintfn "$yellow[%s]: $red[-%s]" cord (getBlockName x (getDiffResultFstValue !~diffResult.data))
                                    true
                                | _ -> true
                                |>fun printed ->
                                    match !~diffResult.dataTag with
                                    |Same _|NN -> ()
                                    |x ->
                                        if printed=false then colorprintf "$yellow[%s: %s]" cord (getBlockName (getDiffResultFstValue !~diffResult.blockID)(getDiffResultFstValue !~diffResult.data))
                                        x
                                        |>NBT.NBTDiffResultToStringTree (true,false) ""
                                        |>formatStringTree "  "
                                        |>Seq.fold (fun state x -> state + x + "\n" ) ""
                                        |>fun x -> x.Replace("]","\]").Replace("\]\]","]").Replace("\n]","]").Replace("[\n","[")
                                        |>ColorPrintFormat
                                        |>colorprintf
                                  
                            ) x
                    | None -> ()
                )
        
        duration f |> ignore
          
        0 // return an integer exit code

