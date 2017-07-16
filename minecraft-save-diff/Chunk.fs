module Chunk
open Utils

type Section = {
    Y: byte;
    Blocks: byte[];
    Add: byte[] option;
    Data: byte[];
    BlockLight: byte[];
    SkyLight: byte[];
}

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
    Sections: Section[];
    Entities: NBT.Payload[];
    TileEntities: NBT.Payload[];
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
                findMap "Sections"
                |>Array.map NBT.Payload.get
                |>Array.map
                    (fun map ->                                        
                       let findMap name = find map name
                       let tryFindMap name = tryFind map name
                       {
                           Y= findMap "Y"
                           Blocks= findMap "Blocks" 
                           Add= tryFindMap "Add" |> Option.map Utils.bytes2Nibbles
                           Data= findMap "Data" |> Utils.bytes2Nibbles
                           BlockLight= findMap "BlockLight" |> Utils.bytes2Nibbles
                           SkyLight= findMap "SkyLight" |> Utils.bytes2Nibbles
                       }
                    )
            Entities= findMap "Entities"
            TileEntities= findMap "TileEntities"
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

type Cord = {
    x: int;
    y: int;
    z: int;
}    

let pos2Cord pos =
    let y = pos &&& 0xF00
    let z = pos &&& 0x0F0
    let x = pos &&& 0x00F
    (x,y,z)

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
    |>Array.collect
        (fun section ->
            let blockIDs = 
                match section.Add with
                | Some add -> Array.map2 (fun block add -> int block + ((int add)<<<8) ) section.Blocks add
                | None -> Array.map int section.Blocks                
            Array.map createBlocks blockIDs
            |>Array.map2 (|>) section.Data
            |>Array.map2 (|>) section.BlockLight
            |>Array.map2 (|>) section.SkyLight
            |>Array.mapi
                (fun pos block ->
                    let (x,y,z) = pos2Cord pos
                    let cord = {
                        x = x + chunk.xPos*16;
                        z = z + chunk.zPos*16;
                        y = y + (int section.Y)*16;
                    }
                    (cord,(block <| Map.tryFind cord tileEntities))
                )
        )
    |>Map.ofArray

type BlockDiffResultRecord = {
    blockID: int DiffResult;
    data: byte DiffResult; 
    blockLight: byte DiffResult;
    skyLight: byte DiffResult;
    dataTag: NBT.NBTDiffResult;
}

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