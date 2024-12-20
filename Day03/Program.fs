﻿open Prelude.Prelude
open FParsec

type Token =
    | Mul of int * int
    | Do
    | Don't

let resolve t =
    match t with
    | Mul(x, y) -> x * y
    | _ -> 0

let lexMul: Parser<Token, unit> =
    tuple2 (pstring "mul(" >>. pint32) (between (pchar ',') (pchar ')') pint32)
    |>> Mul

let lexDo: Parser<Token, unit> = pstring "do()" >>% Do

let lexDon't: Parser<Token, unit> = pstring "don't()" >>% Don't

let lexValid = List.map attempt [ lexMul; lexDo; lexDon't ] |> choice |>> Some

let lexNoOp: Parser<Option<Token>, unit> = anyChar >>% None

let lexToken: Parser<Option<Token>, unit> = lexValid <|> lexNoOp

let lexLine = many lexToken |>> List.collect Option.toList

let parseLine (s: string) =
    match run lexLine s with
    | Success(result, _, _) -> result
    | _ -> failwith "parse error"

let parseFile xs = Seq.collect parseLine xs

let part1 (toks: seq<Token>) = Seq.map resolve toks |> Seq.sum

let part2 (toks: seq<Token>) =
    let mutable latch = true

    let filterHelper (t: Token) =
        match t with
        | Mul _ -> latch
        | Do ->
            latch <- true
            false
        | Don't ->
            latch <- false
            false

    Seq.filter filterHelper toks |> part1


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
