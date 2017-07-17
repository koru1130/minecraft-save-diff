module Chunk
open Utils

[<Struct>]
type Cord = {
    x: int;
    y: int;
    z: int;
}

[<Struct>]
type Pos = {
    xPos: int;
    zPos: int;
    index: int;
    Y: byte;
}

[<Struct>]
type Section = {
    Y: byte;
    BlockID: int[];
    Data: byte[];
    BlockLight: byte[];
    SkyLight: byte[];
}

[<Struct>]
type Chunk = {
    DataVersion: int;
    xPos: int;
    zPos: int;
    LastUpdate: int64;
    LightPopulated: byte;
    TerrainPopulated: byte;
    InhabitedTime: int64;
    Biomes: byte[] option;
    HeightMap: int[];
    Sections: Map<byte,Section> Lazy;
    Entities: NBT.Payload[];
    TileEntities: Map<Cord,NBT.Payload>;
    TileTicks: NBT.Payload[] option;                
}

let find map name = 
    match Map.tryFind name map with
    | Some x -> x
    | None -> failwithf "error: not found %s" name
    |> (!) |> NBT.Payload.get
let tryFind map name = Map.tryFind name map |> Option.map ((!) >> NBT.Payload.get)
let parse nbt =
    match nbt with
    | NBT.Payload.Compound map ->
        let DataVersion = find map "DataVersion"
        let map = find map "Level"
        let findMap name = find map name
        let tryFindMap name = tryFind map name
        {
            DataVersion= DataVersion
            xPos= findMap "xPos"
            zPos= findMap "zPos"
            LightPopulated= findMap "LightPopulated"
            LastUpdate= findMap "LastUpdate"
            TerrainPopulated= findMap "TerrainPopulated"
            InhabitedTime= findMap "InhabitedTime"
            Biomes= tryFindMap "Biomes"
            HeightMap= findMap "HeightMap"
            Sections=
                lazy 
                (
                    findMap "Sections"
                    |>Array.map NBT.Payload.get
                    |>Array.map
                        (fun map ->                                        
                           let findMap name = find map name
                           let tryFindMap name = tryFind map name
                           let (blocks:byte[]) = findMap "Blocks"
                           let Add = tryFindMap "Add" |> Option.map Utils.bytes2Nibbles
                           
                           let result = {
                               Y= findMap "Y"
                               BlockID= 
                                   match Add with
                                   | Some add -> Array.map2 (fun block add -> int block + ((int add)<<<8) ) blocks add
                                   | None -> Array.map int blocks
                               Data= findMap "Data" |> Utils.bytes2Nibbles
                               BlockLight= findMap "BlockLight" |> Utils.bytes2Nibbles
                               SkyLight= findMap "SkyLight" |> Utils.bytes2Nibbles
                           }
                           (result.Y,result)
                        )
                    |>Map.ofArray
                )
            Entities= findMap "Entities"
            TileEntities= 
                findMap "TileEntities"
                |>Array.map
                    (fun nbt ->
                        let findNBT = find <| NBT.Payload.get nbt
                        (
                            {
                                x = findNBT "x"
                                y = findNBT "y"
                                z = findNBT "z"
                            },nbt)
                    )
                |>Map.ofArray
            TileTicks= tryFindMap "TileTicks"
        }
    | _ -> failwith "not Compound"
let tryParse nbt =
    try
        Some(parse nbt)
    with
    | x -> 
        printfn "Error: %A" x
        None
[<Struct>]
type Block = {
    blockID: int;
    data: byte; 
    blockLight: byte;
    skyLight: byte;
    dataTag: NBT.Payload option;
}        
let createBlocks blockID data blockLight skyLight dataTag = 
    {
        blockID=blockID;
        data=data;
        blockLight=blockLight;
        skyLight=skyLight;
        dataTag=dataTag;
    }

let pos2Cord xPos zPos =
    let addX = xPos*16
    let addZ = zPos*16
    fun Y ->
        let addY = (int Y)*16
        fun index ->
            let y = ((index &&& 0xF00) >>> 8) + addY
            let z = ((index &&& 0x0F0) >>> 4) + addZ
            let x = (index &&& 0x00F) + addX
            {x=x;y=y;z=z}

let cord2Pos cord =
    let (xPos,x) = divRem cord.x 16
    let (zPos,z) = divRem cord.z 16
    let (Y,y) = divRem cord.y 16
    let index = y*256+z*16+x
    {
        xPos=xPos;
        zPos=zPos;
        index=index;
        Y=byte Y;
    }
(*
let getBlocks chunk =
    let tileEntities =
        chunk.TileEntities
        |>Array.map
            (fun nbt ->
                let findNBT = find <| NBT.Payload.get nbt
                (
                    {
                        x = findNBT "x"
                        y = findNBT "y"
                        z = findNBT "z"
                    },nbt)
            )
        |>Map.ofArray

    chunk.Sections
    |>Seq.ofArray
    |>Seq.collect
        (fun section ->
            let blockIDs = 
                match section.Add with
                | Some add -> Array.map2 (fun block add -> int block + ((int add)<<<8) ) section.Blocks add
                | None -> Array.map int section.Blocks                
            Seq.map3 createBlocks blockIDs section.Data section.BlockLight
            |>Seq.mapi2            
                (fun i skyLight block->
                    let pos = {
                            index=i;
                            xPos=chunk.xPos;
                            zPos=chunk.zPos;
                            Y=section.Y;
                        }
                    (pos,( (block skyLight) <| None))//Map.tryFind cord tileEntities))
                ) section.SkyLight
        )
    |>Map.ofSeq

type BlockDiffResult =
| Change of BlockDiffResultRecord
| Add of Block
| Del of Block

let diffBlock (lhs:Block) rhs =
    match lhs=rhs with
    |true -> None
    |false -> 
        {
            blockID=diff lhs.blockID rhs.blockID
            data=diff lhs.data rhs.data
            blockLight=diff lhs.blockLight rhs.blockLight
            skyLight= diff lhs.skyLight rhs.skyLight
            dataTag = NBT.diffOption lhs.dataTag rhs.dataTag
        }
        |>Some
        
let diffBlocks lhs rhs =
    match lhs = rhs with
    |false ->
        (keysSet lhs) + (keysSet rhs)
        |>Set.toSeq
        |>Seq.choose
            (fun key ->        
                 match (Map.tryFind key lhs),(Map.tryFind key rhs) with
                 |(Some l , Some r) -> Option.map Change (diffBlock l r)           
                 |(Some l , None) -> Some <| Del l
                 |(None , Some r) -> Some <| Add r
                 |(None , None) -> failwith "error key"
                 |>Option.map (fun x->(key,x))
            )
         |>Map.ofSeq
         |>Some
    |true -> None
    
type BlockDiffResult = 
| Diff of BlockDiffResult
| Add of Section
| Del of Section
| Same of Section

type SectionDiffResult =
| Diff of BlockDiffResult
| Add of Section
| Del of Section
| Same of Section
| NN 
*)
type DiffResultTandTuple<'T> = DiffResult<'T,('T*'T)>

[<Struct>]
type BlockDiffResultRecord = {
    blockID: DiffResultTandTuple<int> Lazy;
    data: DiffResultTandTuple<byte>  Lazy;
    blockLight: DiffResultTandTuple<byte> Lazy;
    skyLight: DiffResultTandTuple<byte> Lazy;
    dataTag: NBT.NBTDiffResult Lazy;
}

let createBlockDiffResult blockID data blockLight skyLight dataTag =
    {
        blockID=blockID;
        data=data;
        blockLight=blockLight;
        skyLight=skyLight;
        dataTag=dataTag;
    }

let diffChunk chunk1 chunk2 =
    let pos2CordXZ = pos2Cord chunk1.xPos chunk1.zPos
    let (sec1,sec2) = (!~chunk1.Sections,!~chunk2.Sections)
    (keysSet sec1) + (keysSet sec2)
    |>Set.toSeq
    |>Seq.map
        (fun key ->
            let index2Cord = pos2CordXZ key            
            match diffOption (Map.tryFind key sec1) (Map.tryFind key sec2) with
            |Diff (l,r) -> 
                let blockIDDiff =
                    Array.map2
                        (fun l r -> 
                            lazy 
                                match l,r with
                                | (l,r) when l=r -> Same l
                                | (0,r) -> Add r
                                | (l,0) -> Del l
                                | (l,r) -> Diff (l,r)
                        ) l.BlockID r.BlockID                                             
                let dataDiff = Array.map2 lazyDiff l.Data r.Data
                let blockLightDiff = Array.map2 lazyDiff l.BlockLight r.BlockLight
                let skyLightDiff = Array.map2 lazyDiff l.SkyLight r.SkyLight
                Array.init 4096 
                    (fun i -> 
                        let cord = index2Cord i
                        let dataTagDiff = lazy (NBT.diffOption (Map.tryFind cord chunk1.TileEntities) (Map.tryFind cord chunk2.TileEntities))
                        (
                            cord,
                            createBlockDiffResult
                                blockIDDiff.[i]
                                dataDiff.[i]
                                blockLightDiff.[i]
                                skyLightDiff.[i]
                                dataTagDiff
                        )
                    )
                |>Diff
            |Add x -> Add (key,x)
            |Del x -> Del (key,x)
            |Same x -> Same (key,x)
            |NN -> failwith "WTF"
        )
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