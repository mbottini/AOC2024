open Prelude.Prelude

type Direction =
    | N
    | S
    | E
    | W

let turn d =
    match d with
    | N
    | S -> [ E; W ]
    | E
    | W -> [ N; S ]

[<CustomComparison>]
[<CustomEquality>]
type Vertex =
    { character: char
      edges: Map<Direction, Option<Vertex>> ref
      id: int }

    interface System.IComparable with
        member self.CompareTo(obj: obj) : int =
            match obj with
            | :? Vertex as { id = x } -> self.id.CompareTo x
            | _ -> -1

    override self.Equals(obj) =
        match obj with
        | :? Vertex as { id = x } -> self.id = x
        | _ -> false

    override self.GetHashCode() : int = self.id.GetHashCode()


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
        for ((_, n, _), (w, o, e), (_, s, _)) in Seq.zip3 (triplewise ante) (triplewise curr) (triplewise post) do
            yield (o, List.zip [ N; W; E; S ] [ n; w; e; s ] |> Map)
    }

let removeNone xs = Seq.collect (Option.toList) xs

let getGridNeighbors xss =
    createGrid xss
    |> triplewise
    |> Seq.collect (uncurry3 getNeighbors)
    |> Seq.map (fun (o, st) -> (o.Value, st))

let parseFile xs =
    let idRef = ref 0

    let getID (x: int ref) =
        let res = x.Value
        x.Value <- x.Value + 1
        res

    let vertices =
        Seq.map
            (Seq.map (fun c ->
                { character = c
                  edges = ref Map.empty
                  id = getID idRef })
             >> Seq.toList)
            xs
        |> Seq.toList

    for (v, vs) in getGridNeighbors vertices do
        v.edges.Value <- vs

    List.concat vertices

let sameRegion c { character = c' } = c = c'

let floodVisit ({ character = c; edges = edges } as v) =
    Seq.filter (Option.map (sameRegion c) >> Option.defaultValue false) (Map.values edges.Value)
    |> Seq.collect Option.toList
    |> cons v

let floodFill v =
    iterate (Seq.collect floodVisit >> Set) (Set.singleton v)
    |> Seq.pairwise
    |> Seq.skipWhile (uncurry (<>))
    |> Seq.head
    |> fst

let allRegions vs =
    let mutable visited = Set.empty

    seq {
        for v in vs do
            if not (Set.contains v visited) then
                let region = floodFill v
                yield region
                visited <- Set.union visited region
    }

let isOtherRegion c opt =
    Option.filter (sameRegion c) opt |> Option.isSome |> not

let area vs = Set.count vs

let vertexFences v =
    let dirs =
        Map.filter (fun _ v' -> isOtherRegion v.character v') v.edges.Value |> Map.keys

    Seq.zip dirs (repeat v)

let adjacentFence (d1, v1) (d2, v2) =
    let helper k v =
        (List.contains k (turn d1)) && v = Some v2

    d1 = d2 && (Map.tryFindKey helper v1.edges.Value |> Option.isSome)

let fenceVisit fences (d, v) =
    turn d
    |> Seq.map (fun d' -> Map.find d' v.edges.Value)
    |> Seq.collect (Option.toList)
    |> Seq.map (fun v' -> (d, v'))
    |> Seq.filter ((flip Set.contains) fences)
    |> cons (d, v)

let floodFence fences (d, v) =
    iterate ((Seq.collect (fenceVisit fences)) >> Set) (Set [ (d, v) ])
    |> Seq.pairwise
    |> Seq.skipWhile (uncurry (<>))
    |> Seq.head
    |> fst
    |> Set

let allFences fences =
    let mutable visited = Set.empty

    seq {
        for (d, v) in fences do
            if not (Set.contains (d, v) visited) then
                let region = floodFence fences (d, v)
                yield region
                visited <- Set.union visited region
    }

let fences vs = Seq.collect vertexFences vs

let perimeter vs = fences vs |> Seq.length

let perimeter2 vs =
    fences vs |> Set |> allFences |> Seq.length

let price1 vs = area vs * perimeter vs

let price2 vs = area vs * perimeter2 vs

let part1 vs =
    allRegions vs |> Seq.map price1 |> Seq.sum

let part2 vs =
    allRegions vs |> Seq.map price2 |> Seq.sum

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
