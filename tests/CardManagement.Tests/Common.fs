module CardManagement.Tests.Common

open FsCheck

// http://www.fssnip.net/7Vo/title/Chunk-String-by-Size-into-List-of-String-Chunks
/// Chunk String by Size into List of String Chunks
let chunkStr size str =
    let rec loop (s:string) accum =
        let branch = size < s.Length
        match branch with
        | true  -> loop (s.[size..]) (s.[0..size-1]::accum)
        | false -> s::accum
    (loop str []) |> List.rev

let ``16DigitsStringGen`` =
    Gen.choose (0,9)
    |> Gen.listOfLength 16
    |> Gen.map (List.map string >> String.concat "")

let insertEvery n str = chunkStr n >> String.concat str

/// generates pair ("0000000000000000", "0000 0000 0000 0000")
type ``16DigitsStringAndWithSpacesEvery4Digits`` =
    static member String() =
        let insertSpacesEvery4Digits = " " |> insertEvery 4
        ``16DigitsStringGen``
        |> Gen.map (fun x -> x,x |> insertSpacesEvery4Digits)
        |> Arb.fromGen
