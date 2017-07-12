namespace MinecraftSaveDiff

module Chunk =
    
    type Block = {
        blockID: int;
        data: int;
        dataTag: NBT.Payload;
    }

    type Light = {
        blockLight: int;
        skyLight: int;
    }

    type Blocks = Map<int*int*int,Block*Light> // (x,y,z)    

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
                    |>Array.map (fun map ->
                                        
                                    let findMap name = find map name
                                    let tryFindMap name = tryFind map name
                                    {
                                        Y= findMap "Y"
                                        Blocks= findMap "Blocks"
                                        Add= tryFindMap "Add"
                                        Data= findMap "Data"
                                        BlockLight= findMap "BlockLight"
                                        SkyLight= findMap "SkyLight"
                                    }
                                )
                Entities= findMap "Entities"
                TileEntities= findMap "TileEntities"
                TileTicks= tryFindMap "TileTicks"
            }
        | _ -> failwith "not Compound"    