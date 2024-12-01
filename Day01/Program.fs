open Prelude.Prelude

let parseLine (s: string) : array<int> = split ' ' s |> Array.map int

let parseFile xs =
    Seq.map parseLine xs |> Seq.transpose |> Seq.map Seq.sort |> Seq.toList

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] ->
        match slurpOrStdin filename |> parseFile with
        | [ xs; ys ] -> Seq.map2 (fun x y -> abs (x - y)) xs ys |> Seq.sum |> printfn "%d"
        | _ -> failwith "Unable to parse file"
    | [| "-p2"; filename |] ->
        match slurpOrStdin filename |> parseFile with
        | [ xs; ys ] ->
            let freqs = frequencies ys

            let helper x =
                x * (Map.tryFind x freqs |> Option.defaultValue 0)

            Seq.map helper xs |> Seq.sum |> printfn "%d"
        | _ -> failwith "Unable to parse file"
    | _ -> failwith "Usage: ./prog -p[1|2] <filename>"

    0
