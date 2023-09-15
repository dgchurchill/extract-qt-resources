module ExtractQtResources.Program

open System
open System.IO
open ExtractQtResources.Parsing
open ExtractQtResources.Parsing.Parser

// Qt resources are compiled by rcc: https://github.com/qt/qtbase/blob/dev/src/tools/rcc/rcc.cpp

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

Array of 14 byte structs (format version 1) or 22 byte structs (format version >= 2).
Nodes are at node_id * sizeof(struct).  i.e. node_id * 14 or node_id * 22.
Root node is has node_id 0.

If flags has directory bit:
| offset | size | description                                                                                        |
|--------+------+----------------------------------------------------------------------------------------------------|
|      0 |    4 | offset of entry in `names` (big endian)                                                            |
|      4 |    2 | flags (1 = compressed, 2 = directory)                                                              |
|      6 |    4 | count of children                                                                                  |
|     10 |    4 | node id of first child (rest of children are sequential from this number, ordered by hash of name) |
|     14 |    8 | last modified timestamp (format version >= 2)                                                       |


Otherwise:
| offset | size | description                                    |
|--------+------+------------------------------------------------|
|      0 |    4 | offset of entry in `names` (big endian)        |
|      4 |    2 | flags (1 = compressed, 2 = directory)          |
|      6 |    2 | country                                        |
|      8 |    2 | language                                       |
|     10 |    4 | offset of entry in payload                     |
|     14 |    8 | last modified timestamp (format version >= 2)  |

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

let parseTreeNode (withTimestamp : bool) : Parser<RawTreeNode> =
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
                    if withTimestamp then
                        let! _ = parseDoubleWord
                        let! _ = parseDoubleWord
                        ()
                    return RawDirectory { ChildCount = childCount; FirstChildId = firstChildId }
                else
                    let! country = parseWord
                    let! language = parseWord
                    let! payloadOffset = parseDoubleWord
                    if withTimestamp then
                        let! _ = parseDoubleWord
                        let! _ = parseDoubleWord
                        ()
                    return RawFile { Country = country; Language = language; PayloadOffset = payloadOffset }
            }

        return {
            RawTreeNode.NameOffset = nameOffset
            Flags = flags
            Data = data
        }
    }

let parseTree (withTimestamp : bool) : Parser<RawTreeNode list> =
    let getMaximumIndex (treeNodes : RawTreeNode list) =
        // printfn "getMaximumIndex: %A" treeNodes
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
                // deal with negatives from overflow if we hit a possible tree node that has a crazy child count
                if extraCount <= 0 then
                    return! fail
                // printfn "currentMaximumIndex: %i" currentMaximumIndex
                // printfn "maximumIndex: %i" maximumIndex
                // printfn "extraCount: %i" extraCount
                let! extra = repeat extraCount (parseTreeNode withTimestamp)
                // printfn "extra: %A" extra
                let result = List.append parsed extra
                return! parseMore result
            }
        else
            unit parsed
            
    parser {
        let! firstNode = (parseTreeNode withTimestamp)
        if firstNode.NameOffset = 0u && (match firstNode.Data with RawDirectory x -> x.FirstChildId = 1u | _ -> false) then
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

let parseXX (offsets : uint32 list) =
    ()

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
        let! rawTreeNodes = parseTree true
        printfn "found tree: %A" rawTreeNodes
        do! seek 0
        let! names = parseNameEntries rawTreeNodes
        printfn "found names: %A" names
        do! seek 0
        let! payloads = parsePayloadEntries rawTreeNodes
        printfn "found payloads: %A" payloads

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
