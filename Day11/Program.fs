#if INTERACTIVE
#r "/home/mike/fsharp/AOC2024/Prelude/bin/Debug/net9.0/Prelude.dll"
#endif

open Prelude.Prelude

let parseLine (xs: string) =
    split ' ' xs |> Seq.map int64 |> Seq.toList

let parseFile xss = Seq.head xss |> parseLine

let splitStone x =
    let s = string x
    let halfLength = s.Length / 2
    [ s.Substring(0, halfLength); s.Substring(halfLength) ] |> List.map int64

let transformStone x =
    match x with
    | 0L -> [ 1L ]
    | n when string n |> Seq.length |> (fun x -> x % 2 = 0) -> splitStone n
    | _ -> [ x * 2024L ]

let rec transformStone2 (memo: Map<int64 * int, int64> ref) n x =
    if Map.containsKey (x, n) memo.Value then
        Map.find (x, n) memo.Value
    else
        let res =
            match n with
            | 0 -> 1L
            | _ ->
                transformStone x
                |> List.map (fun x' -> transformStone2 memo (n - 1) x')
                |> List.sum

        memo.Value <- Map.add (x, n) res memo.Value
        res

let partN n stones =
    let memo: Map<int64 * int, int64> ref = ref Map.empty
    List.map (transformStone2 memo n) stones |> List.sum

let part1 stones = partN 25 stones

let part2 stones = partN 75 stones

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

            lines fh |> parseFile |> f |> printfn "%d"
        | _ -> failwith "usage: ./prog -p[1|2] <filename>"

        0
    with _ as e ->
        printfn "%s %A" e.Message e.StackTrace
        1
