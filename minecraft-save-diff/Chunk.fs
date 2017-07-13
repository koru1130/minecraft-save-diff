module Chunk

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
    dataTag: NBT.Payload option;        
}        
let createBlocks blockID data dataTag = 
    {
        blockID=blockID;
        data=data;
        dataTag=dataTag;
    }

type Light = {
    blockLight: byte;
    skyLight: byte;
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
            let blocks = Array.map2 createBlocks blockIDs section.Data
            let lights = Array.map2 (fun blockLight skyLight -> {blockLight=blockLight; skyLight=skyLight}) section.BlockLight section.SkyLight
            Array.mapi2 
                (fun pos block light ->
                    let (x,y,z) = pos2Cord pos
                    let cord = {
                        x = x + chunk.xPos*16;
                        z = z + chunk.zPos*16;
                        y = y + (int section.Y)*16;
                    }
                    (cord,(block <| Map.tryFind cord tileEntities,light))
                )
                blocks lights
        )
    |>Map.ofArray