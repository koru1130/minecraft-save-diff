module NBT
open Utils

type Name = string

type Payload =        
    | Byte of byte ref
    | Short of int16 ref
    | Int of int ref
    | Long of int64 ref
    | Float of single ref
    | Double of double ref
    | ByteArray of byte[] ref
    | String of string ref
    | List of Payload[] ref
    | Compound of Map<Name,Payload> ref
    | IntArray of int[] ref

    static member get<'T> x =
        let temp =
            match x with 
            | Byte x -> box x
            | Short x -> box x
            | Int x -> box x
            | Long x -> box x
            | Float x -> box x
            | Double x -> box x
            | ByteArray x -> box x
            | String x -> box x
            | List x -> box x
            | Compound x -> box x
            | IntArray x -> box x               

        temp :?> 'T
    static member toStringTree rootName x =
        match x with
        | Compound x ->
            !x
            |>Map.map
                (fun name payload -> Payload.toStringTree name payload)
            |>Map.toList
            |>List.map snd
            |>fun x -> Branch (rootName,x)
        | List x ->
            !x 
            |>Array.mapi (fun i x -> Payload.toStringTree (string i) x)
            |>fun x -> Branch (rootName,List.ofArray x)
        | x -> 
            let str = x.ToString()
            match str with
            |Line x -> Leaf (sprintf "%s: %s" rootName x)
            |Lines x -> Branch (rootName,List.map Leaf x)
    
    //member this.toStringTree(rootName:string) = Payload.toStringTree rootName this
    member this.StringTree = Payload.toStringTree "" this

    //override this.ToString() =
        

type Tag = 
    | End
    | Tag of Name * Payload

type TypeID =
    | TAG_END = 0uy
    | TAG_Byte = 1uy
    | TAG_Short = 2uy
    | TAG_Int = 3uy
    | TAG_Long = 4uy
    | TAG_Float = 5uy
    | TAG_Double = 6uy
    | TAG_Byte_Array = 7uy
    | TAG_String = 8uy
    | TAG_List = 9uy
    | TAG_Compund = 10uy
    | TAG_Int_Array = 11uy

    
type BigEndianBinaryReader(stream: System.IO.Stream) as __ =
    inherit System.IO.BinaryReader(stream)

    let rev = 
        if System.BitConverter.IsLittleEndian then
            Array.rev
        else
            fun arr -> arr
        
    override __.ReadInt32() = 
        let temp = rev <| base.ReadBytes(4)                      
        System.BitConverter.ToInt32(temp,0)
    override __.ReadInt16() = 
        let temp = rev <| base.ReadBytes(2)                      
        System.BitConverter.ToInt16(temp,0)
    override __.ReadInt64() = 
        let temp = rev <| base.ReadBytes(8)                      
        System.BitConverter.ToInt64(temp,0)
    override __.ReadDouble() = 
        let temp = rev <| base.ReadBytes(8)                      
        System.BitConverter.ToDouble(temp,0)
    override __.ReadSingle() =
        let temp = rev <| base.ReadBytes(4)
        System.BitConverter.ToSingle(temp,0)
    override __.ReadUInt32() = 
        let temp = rev <| base.ReadBytes(4)                      
        System.BitConverter.ToUInt32(temp,0)
    override __.ReadUInt16() =
        let temp = rev <| base.ReadBytes(2)
        System.BitConverter.ToUInt16(temp,0)

type Reader(stream: System.IO.Stream) =        
    inherit BigEndianBinaryReader(stream)

    member this.readType() = this.ReadByte()

    member this.readByte() = 
        this.ReadByte() |> ref |> Payload.Byte
    member this.readShort() =
        this.ReadInt16() |> ref |> Payload.Short
    member this.readInt() =
        this.ReadInt32() |> ref |> Payload.Int
    member this.readLong() =
        this.ReadInt64() |> ref |> Payload.Long
    member this.readFloat() =
        this.ReadSingle() |> ref |> Payload.Float
    member this.readDouble() =
        this.ReadDouble() |> ref |> Payload.Double

    member this.readByteArray() =
        let length = this.ReadInt32()
        this.ReadBytes(length)
        |>ref
        |>Payload.ByteArray
    member this.readString() =
        this.readName()
        |>ref
        |>Payload.String        
    member this.readList() =
        let typeId = this.readType()
        let length = this.ReadInt32()
        let arr = Array.zeroCreate length
        for i in 0..(length-1) do
            Array.set arr i <| this.readPayload typeId
        arr |>ref |> Payload.List
    member this.readCompound() =
        let rec loop accList =
            match this.readTag() with
            | Tag.End -> accList
            | Tag.Tag (name,payload) -> loop <| (name,payload) :: accList
        loop []
        |>Map.ofList
        |>ref
        |>Payload.Compound
        
    member this.readIntArray() =
        let length = this.ReadInt32()
        let arr = Array.zeroCreate length
        for i in 0..(length-1) do
            Array.set arr i <| this.ReadInt32()
        arr |>ref |>Payload.IntArray

    member this.readName() = 
        let length = this.ReadUInt16()
        (this.ReadBytes(int length))
        |>System.Text.Encoding.ASCII.GetString

    member this.readPayload typeId =
        match typeId with                
            | 1uy -> this.readByte()
            | 2uy -> this.readShort()
            | 3uy -> this.readInt()
            | 4uy -> this.readLong()
            | 5uy -> this.readFloat()
            | 6uy -> this.readDouble()
            | 7uy -> this.readByteArray()
            | 8uy -> this.readString()
            | 9uy -> this.readList()
            | 10uy -> this.readCompound()
            | 11uy -> this.readIntArray()
            | _ -> failwith "error typeId"
        
    member this.readTag() =
        match this.readType() with
        | 0uy -> Tag.End
        | typeId ->
            let name = this.readName()
            let payload = this.readPayload typeId
            Tag.Tag (name,payload)

let parse (rawNBT:byte[]) =
    let stream = new System.IO.MemoryStream(rawNBT)
    let reader = new Reader(stream)
    match reader.readTag() with
    |Tag.End -> None
    |Tag.Tag(name,payload) -> Some (payload)

//type NBTDiffResult =
//| Same of Payload
//| Diff of lhs:Option<Payload> * rhs:Option<Payload>
//| Comp of Map<Name,NBTDiffResult>
//| List of NBTDiffResult[]
type NBTDiffResult = DiffResult<Payload,Payload*Payload>

let rec NBTDiffResultToStringTree outputAddDel rootName x =
    match x with
    | DiffComp x ->
        x
        |>Map.map (NBTDiffResultToStringTree outputAddDel)
        |>Map.toList
        |>List.map snd
        |>fun x -> Branch (rootName,x)
    | DiffList x ->
        x 
        |>List.mapi (fun i x -> NBTDiffResultToStringTree outputAddDel (string i) x)
        |>fun x -> Branch (rootName,x)
    | Diff (l,r) ->         
        match (l,r) with
        |(Compound _,_)|(_,Compound _)|(Payload.List _,_)|(_,Payload.List _) as x -> 
            let (l,r) = x
            Branch (rootName,[l.StringTree;Leaf " -> ";r.StringTree])
        |(l,r) ->
            match (l.ToString(),r.ToString()) with
            |(Line l,Line r) -> Leaf (rootName + ": " + l+" -> "+r)
            |(Line l,Lines r) -> Branch (rootName,Leaf (l+" ->")::List.map Leaf r)
            |(Lines l,Line r) -> Branch (rootName,List.map Leaf l@[Leaf(" -> "+r)])
            |(Lines l,Lines r) -> Branch (rootName,List.map Leaf l@Leaf " ->"::List.map Leaf r)
    | Del x ->
        match outputAddDel with
        |(_,false) -> Leaf ""
        |(_,true) ->                    
            match x with 
            |(Compound _)|(Payload.List _) as x -> Branch (rootName,[Leaf "-";x.StringTree])
            |x  -> 
                match x.ToString() with
                |Line x -> Leaf (rootName + ": -" + x)
                |Lines x -> Branch (rootName,Leaf "-" :: List.map Leaf x)
    | Add x ->
        match outputAddDel with
        |(false,_) -> Leaf ""
        |(true,_) ->
            match x with 
            |(Compound _)|(Payload.List _) as x -> Branch (rootName,[Leaf "+";x.StringTree])
            |x  -> 
                match x.ToString() with
                |Line x -> Leaf (rootName + ": +" + x)
                |Lines x -> Branch (rootName,Leaf "+" :: List.map Leaf x)    
    |NN -> failwith "WTF"
    | Same _ -> Leaf ""

let rec diff lhs rhs =
    let keysSet = Utils.keysSet
    match (lhs,rhs) with
    |(l,r) when l=r -> Same lhs
    |(Payload.Compound (Ref l),Payload.Compound (Ref r)) -> 
        (keysSet l) + (keysSet r)
        |>Set.toSeq
        |>Seq.map 
            (fun key ->
                let result = 
                   diffOption
                       (Map.tryFind key l)
                       (Map.tryFind key r)
                (key,result)
            )
        |>Map.ofSeq
        |>DiffComp
    |(Payload.List (Ref l),Payload.List (Ref r)) ->        
        let (length,other) = 
            match compare l.Length r.Length with
            | 1 -> (r.Length,Some l.[r.Length..])
            | 0 -> (r.Length,None)
            |(-1) -> (l.Length,Some r.[l.Length..])
            |_-> failwith "error comapre result"
        if length = 0 then Diff(lhs,rhs) else
        Array.map2 diff l.[..length-1] r.[..length-1]
        |>Array.toList
        |>DiffList
        
    |(l,r) -> Diff(l,r)

and diffOption lhs rhs =
    match (lhs,rhs) with
    |(Some l,Some r) when l=r -> Same l
    |(None,Some r) -> Add r
    |(Some l,None) -> Del l
    |(Some l,Some r) -> diff l r
    |(None , None) -> NN

