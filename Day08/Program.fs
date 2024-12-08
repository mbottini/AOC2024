open Prelude.Prelude

type Coord = { x: int; y: int }

type Vector = Vector of Coord

let diff { x = x1; y = y1 } { x = x2; y = y2 } = Vector { x = x2 - x1; y = y2 - y1 }

let scalarMul n (Vector { x = x; y = y }) = Vector { x = n * x; y = n * y }

let inverse (Vector { x = x; y = y }) = Vector { x = (-x); y = (-y) }

let add { x = x1; y = y1 } (Vector { x = x2; y = y2 }) = { x = x1 + x2; y = y1 + y2 }

let antinodes n c1 c2 =
    let vec = diff c1 c2 |> scalarMul n
    [ add c1 (inverse vec); add c2 vec ]

type Antenna = { coord: Coord; value: char }

let parseLine row xs =
    enumerate 0 xs
    |> Seq.filter (snd >> System.Char.IsAsciiLetterOrDigit)
    |> Seq.map (fun (col, c) ->
        { coord = { x = col; y = row }
          value = c })

let parseFile xss =
    let xssLst = Seq.toList xss
    let antennas = enumerate 0 xssLst |> Seq.collect (uncurry parseLine)
    let boardBounds = (Seq.head xssLst |> Seq.length, Seq.length xssLst)
    (antennas, boardBounds)

let insideBounds (x, y) { x = x1; y = y1 } = 0 <= x1 && x1 < x && 0 <= y1 && y1 < x

let antinodeLine bounds a1 a2 =
    iterate ((+) 1) 0
    |> Seq.map (fun n -> antinodes n a1 a2)
    |> Seq.takeWhile (List.exists (insideBounds bounds))
    |> Seq.concat
    |> Seq.filter (insideBounds bounds)

let partN antinodeGen antennas (x, y) =
    Seq.toList antennas
    |> combinations 2
    |> Seq.map tuple2
    |> Seq.filter (fun (a1, a2) -> a1.value = a2.value)
    |> Seq.collect (antinodeGen (x, y))
    |> Set
    |> Set.count

let part1 antennas bounds =
    partN (fun bounds (a1, a2) -> antinodes 1 a1.coord a2.coord |> List.filter (insideBounds bounds)) antennas bounds

let part2 antennas bounds =
    partN (fun bounds (a1, a2) -> antinodeLine bounds a1.coord a2.coord) antennas bounds

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
