module NBT

type Name = string

type Payload =        
    | Byte of byte
    | Short of int16
    | Int of int
    | Long of int64
    | Float of single
    | Double of double
    | ByteArray of byte[]
    | String of string
    | List of Payload[]
    | Compound of Map<Name,Ref<Payload>>
    | IntArray of int[]

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
        Payload.Byte <| this.ReadByte()
    member this.readShort() =
        Payload.Short <| this.ReadInt16()
    member this.readInt() =
        Payload.Int <| this.ReadInt32()
    member this.readLong() =
        Payload.Long <| this.ReadInt64()
    member this.readFloat() =
        Payload.Float <| this.ReadSingle()
    member this.readDouble() =
        Payload.Double <| this.ReadDouble()                

    member this.readByteArray() =
        let length = this.ReadInt32()
        this.ReadBytes(length)
        |>Payload.ByteArray
    member this.readString() =
        this.readName()
        |>Payload.String        
    member this.readList() =
        let typeId = this.readType()
        let length = this.ReadInt32()
        let arr = Array.zeroCreate length
        for i in 0..(length-1) do
            Array.set arr i <| this.readPayload typeId
        Payload.List <| arr
    member this.readCompound() =
        let rec loop accList =
            match this.readTag() with
            | Tag.End -> accList
            | Tag.Tag (name,payload) -> loop <| (name,ref payload) :: accList
        loop []
        |>Map.ofList
        |>Payload.Compound
        
    member this.readIntArray() =
        let length = this.ReadInt32()
        let arr = Array.zeroCreate length
        for i in 0..(length-1) do
            Array.set arr i <| this.ReadInt32()
        Payload.IntArray <| arr

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

type NBTDiffResult =
| Same of Payload option
| Diff of lhs:Option<Payload> * rhs:Option<Payload>
| Comp of Map<Name,NBTDiffResult>
| List of NBTDiffResult[]

let rec diff lhs rhs =
    let keysSet = Utils.keysSet
    match (lhs,rhs) with
    |(l,r) when l=r -> Same <| Some lhs
    |(Payload.Compound l,Payload.Compound r) -> 
        (keysSet l) + (keysSet r)
        |>Set.toSeq
        |>Seq.map 
            (fun key ->
                let result = 
                   diffOption
                       (Option.map (!) (Map.tryFind key l))
                       (Option.map (!) (Map.tryFind key r))                     
                (key,result)
            )
        |>Map.ofSeq
        |>Comp
    |(Payload.List l,Payload.List r) ->
        let (length,other) = 
            match compare l.Length r.Length with
            | 1 -> (r.Length,Some l.[r.Length..])
            | 0 -> (r.Length,None)
            |(-1) -> (l.Length,Some r.[l.Length..])
            |_-> failwith "error comapre result"
        if length = 0 then Diff(Some lhs,Some rhs) else
        Array.map2 diff l.[..length-1] r.[..length-1]
        |>List
        
    |(l,r) -> Diff(Some l,Some r)

and diffOption lhs rhs =
    match (lhs,rhs) with
    |(l,r) when l=r -> Same l
    |(None,Some r) -> Diff(None,Some r)
    |(Some l,None) -> Diff(Some l,None)
    |(Some l,Some r) -> diff l r
    |(None , None) -> Same None