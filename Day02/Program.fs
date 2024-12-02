open Prelude.Prelude

let parseLine (s: string) : array<int> = split ' ' s |> Array.map int

let parseFile xs = Seq.map parseLine xs

let isMonotonic xs =
    let pairs = Seq.pairwise xs

    match Seq.tryHead pairs with
    | Some(x, y) ->
        if x < y then
            Seq.forall (fun (x', y') -> x' < y') pairs
        else if x > y then
            Seq.forall (fun (x', y') -> x' > y') pairs
        else
            false
    | None -> true

let smallDiff xs =
    Seq.pairwise xs
    |> Seq.map (fun (x, y) -> abs (x - y))
    |> Seq.forall (fun x -> x >= 1 && x <= 3)

let isSafePart1 xs = isMonotonic xs && smallDiff xs

let isSafePart2 xs =
    let xsLst = Array.toList xs
    pickOne xsLst |> Seq.map snd |> cons xsLst |> Seq.exists isSafePart1

let part1 (xss: seq<array<int>>) =
    Seq.filter isSafePart1 xss |> Seq.length

let part2 (xss: seq<array<int>>) =
    Seq.filter isSafePart2 xss |> Seq.length

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
