namespace ExtractQtResources.Parsing

type Buffer = {
    Array : byte []
    Offset : int
}

type Parser<'a> = Buffer -> ('a * Buffer) option

module Parser =
    let unit (a : 'a) : Parser<'a> =
        fun buffer -> Some (a, buffer)

    let fail : Parser<'a> =
        fun buffer -> None

    let currentOffset : Parser<int> =
        fun buffer -> Some (buffer.Offset, buffer)

    let seek (offset : int) =
        fun buffer -> Some ((), { buffer with Offset = offset })

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
        member x.Combine(a : Parser<unit>, b : Parser<'a>) = bind a (fun () -> b)
        member x.Zero() = unit ()
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
