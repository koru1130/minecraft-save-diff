﻿namespace MinecraftSaveDiff

module NBT =
    
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
        | Compound of Map<Name,Payload>
        | IntArray of int[]
    
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
                | Tag.Tag (name,payload) -> loop <| (name,payload) :: accList
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

        member this.readTag() =
            match this.readType() with
            | 0uy -> Tag.End
            | typeId ->
                let name = this.readName()
                let payload = this.readPayload typeId
                Tag.Tag (name,payload)