namespace Prelude

module Prelude =

    let curry f = fun x y -> f (x, y)
    let curry3 f = fun x y z -> f (x, y, z)

    let uncurry f = fun (x, y) -> f x y

    let uncurry3 f = fun (x, y, z) -> f x y z

    let flip f = fun x y -> f y x
    let ignore2 _ _ = ()

    let split (delim: char) (s: string) =
        s.Split(
            delim,
            System.StringSplitOptions.RemoveEmptyEntries
            ||| System.StringSplitOptions.TrimEntries
        )

    let reMatch regex matcher s =
        (System.Text.RegularExpressions.Regex regex).Match s |> matcher

    let allMatches regex matcher s =
        (System.Text.RegularExpressions.Regex regex).Matches s |> Seq.map matcher

    let rec zip xs ys =
        match xs, ys with
        | x :: xs', y :: ys' ->
            seq {
                yield (x, y)
                yield! zip xs' ys'
            }
        | _ -> Seq.empty

    let rec zipLongest v xs ys =
        match xs, ys with
        | x :: xs', y :: ys' ->
            seq {
                yield (x, y)
                yield! zipLongest v xs' ys'
            }
        | [], y :: ys' ->
            seq {
                yield (v, y)
                yield! zipLongest v [] ys'
            }
        | x :: xs', [] ->
            seq {
                yield (x, v)
                yield! zipLongest v xs' []
            }
        | _ -> Seq.empty

    let pickOne xs =
        let rec helper acc xs =
            match xs with
            | [] -> Seq.empty
            | x :: xs' ->
                seq {
                    yield (x, List.rev acc @ xs')
                    yield! helper (x :: acc) xs'
                }

        helper [] xs

    let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

    let rec repeatedly f =
        seq {
            yield f ()
            yield! repeatedly f
        }

    let product xs = Seq.fold (*) 1 xs

    let intersperse x xs =
        let helper ys =
            Seq.collect
                (fun y ->
                    seq {
                        yield x
                        yield y
                    })
                ys

        if Seq.isEmpty xs then
            Seq.empty
        else
            seq {
                yield Seq.head xs
                yield! helper (Seq.tail xs)
            }

    let intercalate xs xss = intersperse xs xss |> Seq.concat

    let force xs = Seq.fold ignore2 () xs

    let count n = iterate ((+) 1) n

    let enumerate n xs = Seq.zip (count n) xs

    let repeat x = repeatedly (fun () -> x)

    let cycle xs = repeat xs |> Seq.concat

    let lines (stream: System.IO.Stream) =
        let sr = new System.IO.StreamReader(stream)
        repeatedly sr.ReadLine |> Seq.takeWhile ((<>) null)

    let fileOrStdin path =
        match path with
        | "-" -> System.Console.OpenStandardInput 4096
        | _ -> System.IO.File.OpenRead path

    let takeWhile = Seq.takeWhile

    let dropWhile pred xs =
        let mutable flag = false

        seq {
            for x in xs do
                if not (pred x) then
                    flag <- true

                if flag then
                    yield x
        }

    let groupby f xs =
        let mutable curr = []
        let mutable v = None
        let returnF = f >> Some

        seq {
            for x in xs do
                let v' = returnF x

                if v' = v then
                    curr <- x :: curr
                else
                    if not (List.isEmpty curr) then
                        yield (v.Value, List.rev curr)

                    curr <- [ x ]
                    v <- v'

            if not (List.isEmpty curr) then
                yield (v.Value, List.rev curr)
        }

    let take n xs =
        enumerate 0 xs |> takeWhile (fun (idx, _) -> idx < n) |> Seq.map snd

    let drop = Seq.skip

    let foldl = Seq.fold

    let concat = Seq.concat

    let length = Seq.length

    let cons x xs =
        seq {
            yield x
            yield! xs
        }

    let peek f xs =
        seq {
            for x in xs do
                f x
                yield x
        }

    let frequencies xs =
        let increment x =
            Option.orElse (Some 0) x |> Option.map ((+) 1)

        Seq.fold (fun m x -> Map.change x increment m) Map.empty xs
