open Prelude.Prelude
open FSharp.Collections.ParallelSeq

type Coord = { x: int; y: int }

let add { x = x1; y = y1 } { x = x2; y = y2 } = { x = x1 + x2; y = y1 + y2 }

type Direction =
    | N
    | S
    | E
    | W

    member this.Vector: Coord =
        match this with
        | N -> { x = 0; y = (-1) }
        | S -> { x = 0; y = 1 }
        | E -> { x = 1; y = 0 }
        | W -> { x = (-1); y = 0 }

let turnRight d =
    match d with
    | N -> E
    | E -> S
    | S -> W
    | W -> N

type Position = { coord: Coord; direction: Direction }

type Terrain =
    | Floor
    | Obstacle
    | Start

    static member Parse c =
        match c with
        | '.' -> Floor
        | '^'
        | 'v'
        | '>'
        | '<' -> Start
        | '#' -> Obstacle
        | _ -> failwith "parse error"

let accessCoord (board: array<array<'a>>) (coord: Coord) =
    Array.tryItem coord.y board |> Option.bind (Array.tryItem coord.x)

let move board { coord = c; direction = dir } =
    seq {
        for d in iterate turnRight dir do
            let c2 = add c d.Vector
            let tOpt = accessCoord board c2

            if tOpt <> Some Obstacle then
                yield Option.map (fun _ -> { coord = c2; direction = d }) tOpt
    }
    |> Seq.head
    |> Option.map (fun x -> (x, x))

let traverse board start =
    Seq.unfold (move board) start |> cons start

let parseLine xs = Seq.map Terrain.Parse xs |> Seq.toArray

let parseFile xss = Seq.map parseLine xss |> Seq.toArray

let findStart (board: array<array<Terrain>>) =
    seq {
        for y, ts in enumerate 0 board do
            for x, t in enumerate 0 ts do
                if t.IsStart then
                    { coord = { x = x; y = y }
                      direction = N }
    }
    |> Seq.head

let showTraverse board =
    let helper c =
        match c with
        | Floor -> '.'
        | Obstacle -> '#'
        | Start -> '^'

    let mutable boardChars = Array.map (Array.map helper) board
    let p = findStart board
    let ps = traverse board p

    for { direction = d
          coord = { x = x; y = y } } in ps do
        let c =
            match d with
            | N -> '^'
            | S -> 'v'
            | E -> '>'
            | W -> '<'

        boardChars[y][x] <- c

    printfn
        "%d"
        (boardChars
         |> Seq.concat
         |> Seq.filter (fun c -> c <> '#' && c <> '.')
         |> Seq.length)

    Array.map (fun arr -> Array.map string arr |> System.String.Concat) boardChars
    |> join '\n'


let part1 board =
    let p = findStart board
    traverse board p |> Seq.map _.coord |> Set |> Set.count

let isCycle ps =
    Seq.scan (flip Set.add) Set.empty ps |> Seq.pairwise |> Seq.exists (uncurry (=))

let placeObstacle (board: array<array<Terrain>>) { x = x; y = y } =
    let arr = board[y]
    Array.updateAt y (Array.updateAt x Obstacle arr) board

let part2 board =
    let p = findStart board

    traverse board p
    |> Seq.tail
    |> Seq.map (_.coord)
    |> Set
    |> Seq.map (placeObstacle board)
    |> Seq.map (fun b -> traverse b p)
    |> PSeq.filter isCycle
    |> Seq.length


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
