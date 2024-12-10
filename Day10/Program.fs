open Prelude.Prelude

let parseLine (xs: seq<char>) =
    Seq.map (string >> int) xs |> Seq.toArray

let parseFile xss = Seq.map parseLine xss |> Seq.toArray

let access arr x y =
    Array.tryItem y arr |> Option.bind (Array.tryItem x)

let traverse arr (x0, y0) n =
    [ (x0 + 1, y0); (x0 - 1, y0); (x0, y0 + 1); (x0, y0 - 1) ]
    |> List.filter (fun (x, y) -> access arr x y = Some(n + 1))
    |> List.map (fun tup -> (tup, n + 1))

let bfs arr (x, y) =
    iterate (List.collect (uncurry (traverse arr))) [ ((x, y), 0) ] |> Seq.item 9

let find0s arr =
    seq {
        for y, row in enumerate 0 arr do
            for x, col in enumerate 0 row do
                if col = 0 then
                    yield (x, y)
    }

let part1 grid =
    find0s grid |> Seq.map (bfs grid >> Set >> Set.count) |> Seq.sum

let part2 grid =
    find0s grid |> Seq.map (bfs grid >> Seq.length) |> Seq.sum

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
        printfn "%s" e.Message
        1
