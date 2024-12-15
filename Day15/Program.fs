open Prelude.Prelude

type Coord =
    { x: int
      y: int }

    static member (+)({ x = x1; y = y1 }, { x = x2; y = y2 }) = { x = x1 + x2; y = y1 + y2 }
    static member (*)(n, { x = x; y = y }) = { x = n * x; y = y }

type Object =
    | Box
    | Wall
    | Robot

    static member Parse c =
        match c with
        | 'O' -> Some Box
        | '#' -> Some Wall
        | '@' -> Some Robot
        | _ -> None

type Object2 =
    | BoxL
    | BoxR
    | Wall2
    | Robot2

type Direction =
    | N
    | S
    | E
    | W

    member this.Coord =
        match this with
        | N -> { x = 0; y = (-1) }
        | S -> { x = 0; y = 1 }
        | E -> { x = 1; y = 0 }
        | W -> { x = (-1); y = 0 }

    static member Parse c =
        match c with
        | '^' -> N
        | 'v' -> S
        | '>' -> E
        | '<' -> W
        | _ -> failwith "parse error"

let move (coord, board) (dir: Direction) =
    let rec helper board (coord: Coord) (dir: Direction) =
        let o = Map.find coord board
        let newCoord = coord + dir.Coord

        match Map.tryFind newCoord board with
        | None -> Map.remove coord board |> Map.add newCoord o |> Some
        | Some Wall -> None
        | Some Robot
        | Some Box -> helper board newCoord dir |> Option.bind (fun b -> helper b coord dir)

    match helper board coord dir with
    | Some b' -> (coord + dir.Coord, b')
    | _ -> (coord, board)

let move2 (coord, bigBoard) (dir: Direction) =
    let rec moveHorizontal bigBoard coord (dir: Direction) =
        let newCoord = coord + dir.Coord
        let o = Map.find coord bigBoard

        match Map.tryFind newCoord bigBoard with
        | None -> Map.remove coord bigBoard |> Map.add newCoord o |> Some
        | Some Wall2 -> None
        | Some Robot2
        | Some BoxL
        | Some BoxR ->
            moveHorizontal bigBoard newCoord dir
            |> Option.bind (fun b -> moveHorizontal b coord dir)

    let rec moveVerticalSeparate bigBoard coord (dir: Direction) =
        let newCoord = coord + dir.Coord
        let o = Map.find coord bigBoard

        match Map.tryFind newCoord bigBoard with
        | None -> Map.remove coord bigBoard |> Map.add newCoord o |> Some
        | Some Wall2 -> None
        | Some Robot2 -> failwith "How are you moving a robot?"
        | Some BoxL
        | Some BoxR ->
            moveVerticalTogether bigBoard newCoord dir
            |> Option.bind (fun b -> moveVerticalSeparate b coord dir)

    and moveVerticalTogether bigBoard coord (dir: Direction) =
        let newCoord = coord + dir.Coord
        let o = Map.find coord bigBoard

        match Map.tryFind newCoord bigBoard with
        | None -> Map.remove coord bigBoard |> Map.add newCoord o |> Some
        | Some Wall2 -> None
        | Some Robot2 -> failwith "How are you moving a robot?"
        | Some BoxL ->
            let rightCoord = newCoord + E.Coord

            moveVerticalTogether bigBoard rightCoord dir
            |> Option.bind (fun b -> moveVerticalTogether b newCoord dir)
            |> Option.bind (fun b -> moveVerticalSeparate b coord dir)
        | Some BoxR ->
            let leftCoord = newCoord + W.Coord

            moveVerticalTogether bigBoard leftCoord dir
            |> Option.bind (fun b -> moveVerticalTogether b newCoord dir)
            |> Option.bind (fun b -> moveVerticalSeparate b coord dir)

    let moveFunc =
        match dir with
        | N
        | S -> moveVerticalTogether
        | E
        | W -> moveHorizontal


    match moveFunc bigBoard coord dir with
    | Some b' -> (coord + dir.Coord, b')
    | _ -> (coord, bigBoard)

let parseLine y xs =
    Seq.map Object.Parse xs
    |> enumerate 0
    |> Seq.filter (snd >> Option.isSome)
    |> Seq.map (fun (x, oOpt) -> ({ x = x; y = y }, oOpt.Value))
    |> Map

let parseBoard xs =
    enumerate 0 xs |> Seq.map (uncurry parseLine) |> Seq.fold merge Map.empty

let resolveBigBoard b =
    let helper (coord: Coord, v) =
        match v with
        | Wall -> [ (2 * coord, Wall2); ((2 * coord) + E.Coord, Wall2) ]
        | Box -> [ (2 * coord, BoxL); ((2 * coord) + E.Coord, BoxR) ]
        | Robot -> [ (2 * coord, Robot2) ]

    Map.toSeq b |> Seq.collect helper |> Map

let parseDirs xs =
    Seq.concat xs |> Seq.map Direction.Parse |> Seq.toList

let parseFile xs =
    groupby (fun (s: string) -> s.Length <> 0) xs
    |> Seq.filter fst
    |> Seq.map snd
    |> tuple2
    |> fun (bs, ds) -> parseBoard bs, parseDirs ds

let boxCost { x = x; y = y } = 100 * y + x

let part1 b ds =
    let robotPosition = Map.findKey (fun _ (v: Object) -> v.IsRobot) b

    Seq.fold (fun acc d -> move acc d) (robotPosition, b) ds
    |> snd
    |> Map.filter (fun _ (v: Object) -> v.IsBox)
    |> Map.keys
    |> Seq.map boxCost
    |> Seq.sum


let part2 b ds =
    let b = resolveBigBoard b
    let robotPosition = Map.findKey (fun _ (v: Object2) -> v.IsRobot2) b

    Seq.fold (fun acc d -> move2 acc d) (robotPosition, b) ds
    |> snd
    |> Map.filter (fun _ (v: Object2) -> v.IsBoxL)
    |> Map.keys
    |> Seq.map boxCost
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

            lines fh |> parseFile |> uncurry f |> printfn "%d"
        | _ -> failwith "usage: ./prog -p[1|2] <filename>"

        0
    with _ as e ->
        printfn "%s" e.Message
        printfn "%s" e.StackTrace
        1
