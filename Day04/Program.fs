open Prelude.Prelude

type Direction =
    | NW
    | N
    | NE
    | W
    | E
    | SW
    | S
    | SE

let opposite dir =
    match dir with
    | NW -> SE
    | N -> S
    | NE -> SW
    | W -> E
    | E -> W
    | SW -> NE
    | S -> N
    | SE -> NW

type Vertex =
    { character: char
      edges: Map<Direction, Vertex> ref }

type Path =
    { direction: Direction
      nodes: list<Vertex> }

let prependAndAppend x xs =
    seq {
        yield x
        yield! xs
        yield x
    }

let createGrid xss =
    Seq.map (Seq.map Some) xss
    |> Seq.map (prependAndAppend None)
    |> prependAndAppend (repeat None)

let getNeighbors ante curr post =
    seq {
        for ((nw, n, ne), (w, o, e), (sw, s, se)) in Seq.zip3 (triplewise ante) (triplewise curr) (triplewise post) do
            yield (o, Map [ (NW, nw); (N, n); (NE, ne); (W, w); (E, e); (SW, sw); (S, s); (SE, se) ])
    }

let removeNones (map: Map<'k, Option<'v>>) : Map<'k, 'v> =
    Map.toSeq map
    |> Seq.filter (snd >> Option.isSome)
    |> Seq.map (fun (k, vOpt) -> (k, vOpt.Value))
    |> Map

let getGridNeighbors xss =
    createGrid xss
    |> triplewise
    |> Seq.collect (uncurry3 getNeighbors)
    |> Seq.map (fun (o, map) -> (o.Value, removeNones map))

let parseFile xs =
    let vertices =
        Seq.map (Seq.map (fun c -> { character = c; edges = ref Map.empty }) >> Seq.toList) xs
        |> Seq.toList

    for (v, vs) in getGridNeighbors vertices do
        v.edges.Value <- vs

    List.concat vertices

let allPaths (vs: List<Vertex>) =
    List.map List.singleton vs
    |> List.allPairs [ NW; N; NE; W; E; SW; S; SE ]
    |> List.map (fun (dir, lst) -> { direction = dir; nodes = lst })

let filterPaths start path =
    match path.nodes with
    | [ v ] when v.character = start -> true
    | _ -> false

let rec traversePath xs path =
    match xs, path with
    | [], _ -> Some path
    | y :: ys, { direction = dir; nodes = (p :: ps) } ->
        match Map.tryFind dir p.edges.Value with
        | Some p2 when p2.character = y ->
            traversePath
                ys
                { direction = dir
                  nodes = p2 :: p :: ps }
        | _ -> None
    | _ -> failwith "Somehow the path is empty"

let allAs vs =
    List.filter (fun v -> v.character = 'A') vs

let hasMAS dir v =
    [ dir; opposite dir ]
    |> List.collect (fun d -> Map.tryFind d v.edges.Value |> Option.map (_.character) |> Option.toList)
    |> Set
    |> ((=) (Set "MS"))

let hasXMAS v = hasMAS NW v && hasMAS NE v

let part1 (vs: List<Vertex>) : int =
    allPaths vs
    |> List.filter (filterPaths 'X')
    |> List.map (traversePath (Seq.toList "MAS"))
    |> List.collect Option.toList
    |> List.length

let part2 (vs: List<Vertex>) : int =
    allAs vs |> List.filter hasXMAS |> List.length


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
