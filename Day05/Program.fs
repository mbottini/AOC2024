open Prelude.Prelude

type Vertex =
    { value: int
      edges: Map<int, Vertex> ref }

let createEmptyGraph xs =
    Seq.map (fun x -> x, { value = x; edges = ref Map.empty }) xs |> Map

let fillGraph edges graph =
    for (x, y) in edges do
        let v = Map.find x graph
        v.edges.Value <- Map.add y (Map.find y graph) v.edges.Value

    graph

let parseEdges xs =
    let helper (m: System.Text.RegularExpressions.Match) =
        int m.Groups[1].Value, int m.Groups[2].Value

    Seq.map (reMatch @"(\d+)\|(\d+)" helper) xs

let parseGraph xs =
    let edges = parseEdges xs

    Seq.collect (fun (x, y) -> [ x; y ]) edges
    |> Set
    |> createEmptyGraph
    |> fillGraph edges

let parseUpdates xs =
    Seq.map (split ',' >> Array.map int >> Array.toList) xs |> Seq.toList

let parseFile xs =
    match groupby ((<>) "") xs |> Seq.filter fst |> Seq.map snd |> Seq.toList with
    | [ edgeStrs; updateStrs ] -> parseGraph edgeStrs, parseUpdates updateStrs
    | _ -> failwith "parse error"

let depends graph x y =
    let v = (Map.find x graph)
    Map.containsKey y v.edges.Value

let swapDeps graph update =
    let rec helper xs =
        match xs with
        | [] -> []
        | y :: ys ->
            match pickOne ys |> Seq.filter (fun (z, _) -> depends graph y z) |> Seq.tryHead with
            | Some(z, zs) -> helper (z :: y :: zs)
            | None -> y :: helper ys

    helper update |> List.rev

let middle xs =
    let rec helper tortoise hare =
        match tortoise, hare with
        | y :: _, [] -> y
        | y :: _, [ _ ] -> y
        | y :: ys, _ :: _ :: zs -> helper ys zs
        | _ -> failwith "this should never happen"

    helper xs xs

let partN graph updates filterer =
    Seq.map (fun u -> (u, swapDeps graph u)) updates
    |> Seq.filter (uncurry filterer)
    |> Seq.map (snd >> middle)
    |> Seq.sum

let part1 graph updates = partN graph updates (=)

let part2 graph updates = partN graph updates (<>)

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

            lines fh |> parseFile |> (uncurry f) |> printfn "%d"
        | _ -> failwith "usage: ./prog -p[1|2] <filename>"

        0
    with _ as e ->
        printfn "%s" e.Message
        1
