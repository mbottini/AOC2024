open Prelude.Prelude

type Equation = { value: int64; nums: list<int64> }

type Op =
    | Add
    | Mul
    | Concat

type Expr =
    | Const of int64
    | BinOp of BinOp

and BinOp = { op: Op; left: Expr; right: Expr }

let rec eval (e: Expr) =
    match e with
    | Const x -> x
    | BinOp { op = Add; left = left; right = right } -> eval left + eval right
    | BinOp { op = Mul; left = left; right = right } -> eval left * eval right
    | BinOp { op = Concat
              left = left
              right = right } -> sprintf "%d%d" (eval right) (eval left) |> int64



let parseLine xs =
    match split ':' xs with
    | [| valueStr; rest |] ->
        { value = int64 valueStr
          nums = split ' ' rest |> Array.map int64 |> Array.toList }
    | _ -> failwith "parse error"

let parseFile xss = Seq.map parseLine xss

let rec allExprs ops xs =
    match xs with
    | [] -> failwith "No expression"
    | [ x ] -> Seq.singleton (Const x)
    | y :: ys ->
        Seq.allPairs ops (allExprs ops ys)
        |> Seq.map (fun (op, r) -> BinOp { op = op; left = Const y; right = r })


let partN ops eqs =
    Seq.filter
        (fun { value = value; nums = nums } -> allExprs ops (List.rev nums) |> Seq.map eval |> Seq.exists ((=) value))
        eqs
    |> Seq.map (_.value)
    |> Seq.sum

let part1 = partN [ Add; Mul ]

let part2 = partN [ Add; Mul; Concat ]

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
