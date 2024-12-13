open Prelude.Prelude
open FParsec

type Vector =
    { x: int64
      y: int64 }

    static member (+)({ x = x1; y = y1 }, { x = x2; y = y2 }) = { x = x1 + x2; y = y1 + y2 }
    static member (*)({ x = x; y = y }, scalar) = { x = scalar * x; y = scalar * y }
    static member (*)(scalar, (v: Vector)) = v * scalar

let det { x = x1; y = y1 } { x = x2; y = y2 } = x1 * y2 - x2 * y1

let cramer v1 v2 v3 =
    match det v1 v2 with
    | 0L -> None // No solution at all
    | d ->
        let d1 = det v3 v2
        let d2 = det v1 v3

        if d1 % d <> 0 || d2 % d <> 0 then // No integer solution
            None
        else
            Some(d1 / d, d2 / d)

let parseX: Parser<int64, unit> =
    pstring "X" >>. (pstring "+" <|> pstring "=") >>. pint64

let parseY: Parser<int64, unit> =
    pstring "Y" >>. (pstring "+" <|> pstring "=") >>. pint64

let parseVector: Parser<Vector, unit> =
    tuple2 parseX (pstring ", " >>. parseY) |>> fun (x, y) -> { x = x; y = y }

let parseA: Parser<Vector, unit> = pstring "Button A: " >>. parseVector

let parseB: Parser<Vector, unit> = pstring "Button B: " >>. parseVector

let parsePrize: Parser<Vector, unit> = pstring "Prize: " >>. parseVector

let runParser (p: Parser<'a, unit>) (s: string) =
    match run p s with
    | Success(result, _, _) -> result
    | _ -> failwith "parse error"

let parseProblem chunk =
    Seq.map2 runParser [ parseA; parseB; parsePrize ] chunk
    |> Prelude.Prelude.tuple3

let parseFile xs =
    Seq.chunkBySize 4 xs |> Seq.map parseProblem

let tokenCost a b = 3L * a + b

let part1 ps =
    Seq.map (uncurry3 cramer) ps
    |> Seq.map (Option.map (uncurry tokenCost) >> Option.defaultValue 0L)
    |> Seq.sum

let part2 ps =
    let extraVec = 10000000000000L * { x = 1; y = 1 }

    Seq.map (fun (a, b, p) -> (a, b, p + extraVec)) ps |> part1

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
