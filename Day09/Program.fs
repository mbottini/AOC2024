open Prelude.Prelude

type INode = { id: int64; size: int }

type DriveSpace =
    { inodes: list<INode>
      freeSpace: int
      prevSpace: int }

let parseChar c = string c |> int

let parseLine (xs: seq<char>) =
    let helper (idx, (arr: array<char>)) =
        match arr with
        | [| x; y |] ->
            { inodes = [ { id = int64 idx; size = parseChar x } ]
              freeSpace = parseChar y
              prevSpace = 0 }
        | [| x |] ->
            { inodes = [ { id = int64 idx; size = parseChar x } ]
              freeSpace = 0
              prevSpace = 0 }
        | _ -> failwith "this should never happen" in

    Seq.chunkBySize 2 xs |> enumerate 0 |> Seq.map helper

let collate1 dss =
    let mutable (arr: array<DriveSpace>) = Seq.toArray dss

    let rec helper left right =
        if left >= right then
            arr
        else
            match arr[left], arr[right] with
            | { inodes = _; freeSpace = 0 }, _ -> helper (left + 1) right
            | _, ({ inodes = { id = _; size = 0 } :: rest } as ds) ->
                arr[right] <- { ds with inodes = rest }
                helper left right
            | { inodes = ns; freeSpace = n }, { inodes = [ { id = p; size = q } ] } when q > n ->
                arr[left] <-
                    { inodes = { id = p; size = n } :: ns
                      freeSpace = 0
                      prevSpace = 0 }

                arr[right] <-
                    { inodes = [ { id = p; size = q - n } ]
                      freeSpace = 0
                      prevSpace = 0 }

                helper left right
            | { inodes = ns; freeSpace = n }, { inodes = [ { id = p; size = q } ] } ->
                arr[left] <-
                    { inodes = { id = p; size = q } :: ns
                      freeSpace = n - q
                      prevSpace = 0 }

                arr[right] <-
                    { inodes = []
                      freeSpace = 0
                      prevSpace = 0 }

                helper left right
            | _, { inodes = []; freeSpace = _ } -> helper left (right - 1)
            | _ -> failwith "Something bad happened"

    helper 0 (Array.length arr - 1)
    |> Seq.map (fun { inodes = xs; freeSpace = n } ->
        { inodes = List.rev xs
          freeSpace = n
          prevSpace = 0 })

let resolveInode { id = n; size = p } = Seq.replicate p n

let resolveDriveSpace
    { inodes = inodes
      freeSpace = n
      prevSpace = p }
    =
    Seq.concat [ Seq.replicate p 0L; (Seq.collect resolveInode inodes); Seq.replicate n 0L ]

let resolveDrive dss = Seq.collect resolveDriveSpace dss

let checksum (ns: seq<int64>) =
    enumerate 0 ns |> Seq.map (fun (n, x) -> int64 n * x) |> Seq.sum

let collate2 dss =
    let mutable arr = Seq.toArray dss
    let maxIndex = Seq.map (fun { inodes = xs } -> List.head xs |> _.id) arr |> Seq.max

    let rec helper lastID index =
        if index < 0 then
            arr
        else
            match arr[index] with
            | { inodes = xs
                freeSpace = n
                prevSpace = q } when List.tryLast xs |> Option.map _.id = Some lastID ->
                let x, xs = List.last xs, (List.rev xs |> List.tail |> List.rev)

                match
                    [ 0 .. index - 1 ]
                    |> List.filter (fun i -> arr[i].freeSpace >= x.size)
                    |> List.tryHead
                with
                | Some i ->
                    let { inodes = ys; freeSpace = p } = arr[i]

                    arr[i] <-
                        { inodes = x :: ys
                          freeSpace = p - x.size
                          prevSpace = q }

                    arr[index] <-
                        { inodes = xs
                          freeSpace = n
                          prevSpace = q + x.size }

                    helper (lastID - 1L) index
                | None -> helper (lastID - 1L) index
            | _ -> helper lastID (index - 1)

    helper maxIndex (Array.length arr - 1)
    |> Array.map
        (fun
            { inodes = xs
              freeSpace = n
              prevSpace = p } ->
            { inodes = List.rev xs
              freeSpace = n
              prevSpace = p })
    |> Array.toSeq




let parseFile xss = Seq.head xss |> parseLine

let part1 dss =
    collate1 dss |> resolveDrive |> checksum

let part2 dss =
    collate2 dss |> resolveDrive |> checksum

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
