open Prelude.Prelude
open FParsec

type Vector =
    { x: int
      y: int }

    static member (+)({ x = x1; y = y1 }, { x = x2; y = y2 }) = { x = x1 + x2; y = y1 + y2 }
    static member (*)({ x = x; y = y }, scalar) = { x = scalar * x; y = scalar * y }
    static member (*)(scalar, (v: Vector)) = v * scalar

    static member (%)({ x = x1; y = y1 }, { x = x2; y = y2 }) =
        { x = ((x1 % x2) + x2) % x2
          y = ((y1 % y2) + y2) % y2 }

type Robot = { position: Vector; velocity: Vector }

let moveN bounds (n: int) { position = p; velocity = v } =
    { position = (p + n * v) % bounds
      velocity = v }

let quadrant { x = xBound; y = yBound } { position = { x = x; y = y } } =
    let middleX = xBound / 2
    let middleY = yBound / 2

    List.allPairs [ (-1); 1 ] [ (-1); 1 ]
    |> List.tryFindIndex ((=) (x.CompareTo middleX, y.CompareTo middleY))

let parseVector: Parser<Vector, unit> =
    tuple2 pint32 (pstring "," >>. pint32) |>> fun (x, y) -> { x = x; y = y }

let parseChunk: Parser<Vector, unit> =
    (pstring "p" <|> pstring "v") >>. pstring "=" >>. parseVector

let parseRobot: Parser<Robot, unit> =
    tuple2 parseChunk (pstring " " >>. parseChunk)
    |>> fun (p, v) -> { position = p; velocity = v }

let runParser (p: Parser<'a, unit>) (s: string) =
    match run p s with
    | Success(result, _, _) -> result
    | _ -> failwith "parse error"

let parseFile xs = Seq.map (runParser parseRobot) xs

let part1 bounds n rs =
    Seq.map (moveN bounds 100) rs
    |> Seq.map (quadrant bounds)
    |> frequencies
    |> Map.filter (fun k _ -> k.IsSome)
    |> Map.values
    |> Seq.fold (fun x y -> x * y) 1

let singleLine length xs =
    Seq.pairwise xs
    |> groupby (fun (x, y) -> y - x = 1)
    |> Seq.filter fst
    |> Seq.map snd
    |> Seq.exists (fun lst -> List.length lst >= (length - 1))

let findLine rs =
    Seq.groupBy (_.position.y) rs
    |> Seq.map snd
    |> Seq.map (Seq.map _.position.x)
    |> Seq.map (Seq.sort)
    |> Seq.exists (singleLine 31)

let part2 bounds rs =
    iterate (List.map (moveN bounds 1)) (Seq.toList rs) |> Seq.findIndex findLine

[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            let f =
                match flag with
                | "-p1" -> part1 { x = 101; y = 103 } 100
                | "-p2" -> part2 { x = 101; y = 103 }
                | _ -> failwith "invalid flag"

            use fh = fileOrStdin filename

            lines fh |> parseFile |> f |> printfn "%d"
        | _ -> failwith "usage: ./prog -p[1|2] <filename>"

        0
    with _ as e ->
        printfn "%s" e.Message
        1
