module ExtractQtResources

open System
open System.IO

type Buffer = {
    Array : byte []
    Offset : int
}

type Parser<'a> = Buffer -> ('a * Buffer) option

let unit (a : 'a) : Parser<'a> =
    fun buffer -> Some (a, buffer)

let fail : Parser<'a> =
    fun buffer -> None

let currentOffset : Parser<int> =
    fun buffer -> Some (buffer.Offset, buffer)

let skip (n : int) : Parser<unit> =
    fun buffer -> Some ((), { buffer with Offset = buffer.Offset + n })

let rewind (n : int) : Parser<unit> =
    skip -n

let bind (ap : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
    ap >> Option.bind (fun (a, buffer1) -> f a buffer1)

type ParserBuilder() =
    member x.Bind(comp, func) = bind comp func
    member x.Return(value) = unit value
    member x.ReturnFrom(value) = value
    member x.Delay(func) = fun buffer -> func () buffer

let parser =
    ParserBuilder()

let rec repeat n (p : Parser<'a>) : Parser<'a list> =
    parser {
        if n > 0 then
            let! a = p
            let! rest = repeat (n - 1) p
            return a :: rest
        else
            return []
    }

let rec scanFor (p : Parser<'a>) (test : 'a -> bool) : Parser<'a> =
    let rec tryScan (buffer : Buffer) =
        match p buffer with
        | Some (a, _) as result when test a ->
            result
        | _ ->
            if buffer.Offset + 1 < Array.length buffer.Array then
                tryScan { buffer with Offset = buffer.Offset + 1}
            else
                None

    tryScan

let parseWith (f : byte[] -> int -> int -> 'a) (n : int) : Parser<'a> =
    fun (buffer : Buffer) ->
        if buffer.Offset + n > buffer.Array.Length then
            None
        else
            Some (f buffer.Array buffer.Offset n, { buffer with Offset = buffer.Offset + n })

(* Numbers are big-endian *)
let parseDoubleWord : Parser<uint32> =
    let convertUint32 (array : byte[]) (offset : int) _ =
        (uint32 array.[offset] <<< 24)
        ||| (uint32 array.[offset + 1] <<< 16)
        ||| (uint32 array.[offset + 2] <<< 8)
        ||| (uint32 array.[offset + 3])
    
    parseWith convertUint32 4

let parseWord : Parser<uint16> =
    let convertUint16 (array : byte[]) (offset : int) _ =
        (uint16 array.[offset] <<< 8)
        ||| (uint16 array.[offset + 1])
    
    parseWith convertUint16 2

(* Tree:

Array of 14 byte structs.
Nodes are at node_id * 14.
Root node is has node_id 0.

If flags has directory bit:
| offset | size | description                                                                                        |
|--------+------+----------------------------------------------------------------------------------------------------|
|      0 |    4 | offset of entry in `names` (big endian)                                                            |
|      4 |    2 | flags (1 = compressed, 2 = directory)                                                              |
|      6 |    4 | count of children                                                                                  |
|     10 |    4 | node id of first child (rest of children are sequential from this number, ordered by hash of name) |

Otherwise:
| offset | size | description                             |
|--------+------+-----------------------------------------|
|      0 |    4 | offset of entry in `names` (big endian) |
|      4 |    2 | flags (1 = compressed, 2 = directory)   |
|      6 |    2 | country                                 |
|      8 |    2 | language                                |
|     10 |    4 | offset of entry in payload              |

*)

[<Flags>]
type NodeFlag =
    | Compressed = 1us
    | Directory = 2us

type DirectoryData = {
    ChildCount : uint32
    FirstChildId : uint32
}

type FileData = {
    Country : uint16
    Language : uint16
    PayloadOffset : uint32
}

type TreeNodeData =
    | RawDirectory of DirectoryData
    | RawFile of FileData

type RawTreeNode = {
    NameOffset : uint32
    Flags : NodeFlag
    Data : TreeNodeData
}

type TreeNode =
    | Directory of name : string * children : TreeNode list
    | File of name : string * fileOffset : int * length : int

let parseTreeNode : Parser<RawTreeNode> =
    parser {
        let! nameOffset = parseDoubleWord
        let! rawFlags = parseWord
        if rawFlags > 3us then return! fail else
        
        let flags = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint16, NodeFlag>(rawFlags)
        let! data =
            parser {
                if (flags &&& NodeFlag.Directory) = NodeFlag.Directory then
                    let! childCount = parseDoubleWord
                    let! firstChildId = parseDoubleWord
                    return RawDirectory { ChildCount = childCount; FirstChildId = firstChildId }
                else
                    let! country = parseWord
                    let! language = parseWord
                    let! payloadOffset = parseDoubleWord
                    return RawFile { Country = country; Language = language; PayloadOffset = payloadOffset }
            }

        return {
            RawTreeNode.NameOffset = nameOffset
            Flags = flags
            Data = data
        }
    }

let parseTree : Parser<RawTreeNode list> =
    let getMaximumIndex (treeNodes : RawTreeNode list) =
        treeNodes
        |> List.choose (fun node ->
            match node.Data with
            | RawDirectory directory ->
                Some <| directory.FirstChildId + directory.ChildCount - 1u
            | RawFile _ ->
                None)
        |> (fun rest -> (List.length treeNodes - 1 |> uint32) :: rest)
        |> List.max

    let rec parseMore (parsed : RawTreeNode list) =
        let maximumIndex = getMaximumIndex parsed
        let currentMaximumIndex = List.length parsed - 1 |> uint32
        if maximumIndex > currentMaximumIndex then
            parser {
                let extraCount = int (maximumIndex - currentMaximumIndex)
                let! extra = repeat extraCount parseTreeNode
                let result = List.append parsed extra
                return! parseMore result
            }
        else
            unit parsed
            
    parser {
        let! firstNode = parseTreeNode
        if firstNode.NameOffset = 0u && firstNode.Data = RawDirectory { ChildCount = 1u; FirstChildId = 1u } then
            return! parseMore [ firstNode ]
        else
            return! fail
    }


(* Names:

Array of

| offset |    size | description                   |
|--------+---------+-------------------------------|
|      0 |       2 | length of name                |
|      2 |       4 | hash of name (for comparison) |
|      6 | len * 2 | name in UTF-16                |

*)

type NameEntry = {
    Length : uint16
    Hash : uint32
    Name : string
}

let utf16ToString (bytes : byte[]) (offset : int) (length : int) =
    Text.Encoding.BigEndianUnicode.GetString bytes.[offset .. offset + length - 1]

let parseNameEntry : Parser<NameEntry> =
    parser {
        let! length = parseWord
        let! hash = parseDoubleWord
        let! name = parseWith utf16ToString (int length * 2)
        return {
            NameEntry.Length = length
            Hash = hash
            Name = name
        }
    }

let parseNameEntries (tree : RawTreeNode list) : Parser<NameEntry list> =
    parser {
        let nameEntryOffsets =
            tree
            |> Seq.map (fun node -> node.NameOffset)
            |> Seq.distinct
            |> Seq.sort
            |> List.ofSeq

        let nameEntryCount =
            List.length nameEntryOffsets  

        // Could probably to find the name table anyway, but there should hopefully always be at least two names
        if nameEntryCount < 2 then return! fail else

        let firstEntrySize = nameEntryOffsets.[1] - nameEntryOffsets.[0]
        let expectedNameByteLength = int firstEntrySize - 6 // full size, less size of length and hash fields
        if expectedNameByteLength % 2 <> 0 || expectedNameByteLength < 0 then return! fail else

        let expectedNameLength = uint16 (expectedNameByteLength / 2)
        let! _ = scanFor parseWord ((=) expectedNameLength)
        do! rewind 2

        return! repeat nameEntryCount parseNameEntry
    }
        

(* Payloads:

Array of

| offset | size   | description |
|--------+--------+-------------|
|      0 | 4      | length      |
|      4 | length | data        |

*)

type PayloadEntry = {
    Length : uint32
    FileOffset : int
}

let parsePayloadEntry : Parser<PayloadEntry> =
    parser {
        let! length = parseDoubleWord
        let! fileOffset = currentOffset
        do! skip (int length)
        return {
            PayloadEntry.Length = length
            FileOffset = fileOffset
        }
    }

let parsePayloadEntries (tree : RawTreeNode list) : Parser<PayloadEntry list> =
    parser {
        let payloadOffsets =
            tree
            |> Seq.choose (fun node ->
                match node.Data with
                | RawFile file -> Some file.PayloadOffset
                | _ -> None)
            |> Seq.distinct
            |> Seq.sort
            |> List.ofSeq

        let payloadCount =
            List.length payloadOffsets  

        // Could probably to find the payload table anyway, but there should hopefully always be at least two payloads
        if payloadCount < 2 then return! fail else

        let firstEntrySize = payloadOffsets.[1] - payloadOffsets.[0]
        let expectedLength = int firstEntrySize - 4 // full size, less size of length field
        if expectedLength < 0 then return! fail else

        let! _ = scanFor parseDoubleWord ((=) (uint32 expectedLength))
        do! rewind 4

        return! repeat payloadCount parsePayloadEntry
    }

let parseResources : Parser<TreeNode> =
    parser {
        let! rawTreeNodes = parseTree
        let! names = parseNameEntries rawTreeNodes
        let! payloads = parsePayloadEntries rawTreeNodes

        let nameOffsets =
            rawTreeNodes
            |> Seq.map (fun node -> node.NameOffset)
            |> Seq.distinct
            |> Seq.sort

        let namesByOffset =
            Seq.zip nameOffsets names
            |> Map.ofSeq

        let payloadOffsets =
            rawTreeNodes
            |> Seq.choose (fun node ->
                match node.Data with
                | RawFile file -> Some file.PayloadOffset
                | _ -> None)
            |> Seq.distinct
            |> Seq.sort

        let payloadsByOffset =
            Seq.zip payloadOffsets payloads
            |> Map.ofSeq

        let rawTreeNodeArray =
            List.toArray rawTreeNodes

        let rec buildTreeNode (childId : int) : TreeNode =
            let rawTreeNode = rawTreeNodeArray.[childId]
            let name =
                namesByOffset
                |> Map.find rawTreeNode.NameOffset

            match rawTreeNode.Data with
            | RawDirectory directory ->
                let childNodes =
                    seq { int directory.FirstChildId .. int directory.FirstChildId + int directory.ChildCount - 1 }
                    |> Seq.map buildTreeNode
                    |> List.ofSeq
                Directory (name.Name, childNodes)
            | RawFile file ->
                let payload =
                    payloadsByOffset
                    |> Map.find file.PayloadOffset

                File (name.Name, payload.FileOffset, int payload.Length)

        return buildTreeNode 0
    }

    // | Directory of name : string * children : TreeNode list
    // | File of name : string * fileOffset : int * length : int
let traverseTree (tree : TreeNode) : (string * TreeNode) seq =
    let rec traverse (node : TreeNode) (path : string) =
        seq {
            let nodeName =
                match node with
                | Directory (name, _) -> name
                | File (name, _, _) -> name

            let fullPath =
                Path.Combine(path, nodeName)

            yield (fullPath, node)

            match node with
            | Directory (_, children) ->
                for child in children do
                    yield! traverse child fullPath
            | _ ->
                ()
        }

    traverse tree ""

let listResources (tree : TreeNode) =
    for (path, node) in traverseTree tree do
        match node with
        | Directory _ ->
            printfn "%s/" path
        | File _ ->
            printfn "%s" path

let extractResources (tree : TreeNode) (buffer : byte []) =
    for (path, node) in traverseTree tree do
        match node with
        | Directory _ ->
            printfn "%s/" path
            Directory.CreateDirectory path |> ignore
        | File (_, fileOffset, length) ->
            printfn "%s" path
            File.WriteAllBytes(path, buffer.[fileOffset .. fileOffset + length - 1])

[<EntryPoint>]
let main argv =
    if Array.length argv <> 2 then
        printfn """Usage: ExtractQtResources extract|list <filename>

WARNING: No santization of directory / filenames is done.
WARNING: Files will be blindly overwritten.

List the resources first to make sure everything looks safe."""
        1
    else
        let isExtracting = argv.[0] = "extract"
        let filename = argv.[1]
        let bytes = File.ReadAllBytes filename

        let result =
            scanFor parseResources (fun _ -> true) { Buffer.Array = bytes; Offset = 0 }

        match result with
        | Some (tree, _) ->
            if isExtracting then
                extractResources tree bytes
            else
                listResources tree
            0

        | None ->
            printfn "No resources found."
            1
