open Prelude.Prelude

let parseLine (s: string) : array<int> = split ' ' s |> Array.map int

let parseFile xs =
    Seq.map parseLine xs |> Seq.transpose |> Seq.map Seq.sort |> Seq.toList

let part1 (xs: seq<int>) (ys: seq<int>) : int =
    Seq.map2 (fun x y -> abs (x - y)) xs ys |> Seq.sum

let part2 (xs: seq<int>) (ys: seq<int>) : int =
    let freqs = frequencies ys

    Seq.map (fun x -> x * (Map.tryFind x freqs |> Option.defaultValue 0)) xs
    |> Seq.sum

[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            let f =
                match flag with
                | "-p1" -> part1
                | "-p2" -> part2
                | _ -> failwith "invalid flag"

            use fh = fileOrStdin filename

            match lines fh |> parseFile with
            | [ xs; ys ] -> f xs ys |> printfn "%d"
            | _ -> failwith "parse error"
        | _ -> failwith "usage: ./prog -p[1|2] <filename>"

        0
    with _ as e ->
        printfn "%s" e.Message
        1
