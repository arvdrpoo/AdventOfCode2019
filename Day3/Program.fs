// Learn more about F# at http://fsharp.org

type WireSegment =
    | U of int
    | D of int
    | L of int
    | R of int

type Coord = {X:int; Y:int}

let createWireFromSegmentString (s:string) =
    match s.[0] with
    | 'U' -> U (s.Substring(1)|>int)
    | 'D' -> D (s.Substring(1)|>int)
    | 'L' -> L (s.Substring(1)|>int)
    | 'R' -> R (s.Substring(1)|>int)
    | _ -> failwithf "unknown direction %s" s

let addSegmentPart (board:Map<Coord, 'a list>) coord wireId =
    match board.TryFind coord with
    | Some x -> board.Add(coord, List.append([wireId]) x)
    | None -> board.Add(coord, [wireId])

let addWireSegmentToBoard (board:Map<Coord, int list>) startCoord wireId (segment:WireSegment)=
    let mutable b = board
    match segment with
    | U (l) ->
        for y = (startCoord.Y+1) to (startCoord.Y + l) do
            b <- addSegmentPart b {X=startCoord.X;Y=y} wireId
        (b, {X=startCoord.X;Y=startCoord.Y+l})
    | D (l) ->
        for y = (startCoord.Y-1) downto (startCoord.Y - l) do
            b <- addSegmentPart b {X=startCoord.X;Y=y} wireId
        (b,{X=startCoord.X;Y=startCoord.Y-l})
    | L (l) ->
        for x = (startCoord.X-1) downto (startCoord.X - l) do
            b <- addSegmentPart b {X=x;Y=startCoord.Y} wireId
        (b,{X=startCoord.X-l;Y=startCoord.Y})
    | R (l) ->
        for x = (startCoord.X+1) to (startCoord.Y + l) do
            b <- addSegmentPart b {X=x;Y=startCoord.Y} wireId
        (b, {X=startCoord.X+l;Y=startCoord.Y})

let distance p1 p2 =
    abs(p1.X - p2.X) + abs(p1.Y - p2.Y)

let distanceFromOrigin =
    distance {X=0;Y=0;}

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "input.txt"
                |> Seq.map (fun a -> a())
                |> Seq.map (fun l ->
                                l.Split(",")
                                |> Seq.map(fun op -> createWireFromSegmentString op ))

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds

    // part 1
    stopwatch.Restart()
    let mutable board = Map.empty.Add({X=0;Y=0;}, List.init (Seq.length input) (fun index -> index))

    input
    |> Seq.iteri (fun wireId wire ->
        wire
        |> Seq.fold (fun state wireseg ->
            wireseg
            |> addWireSegmentToBoard (fst state) (snd state) wireId) (board, {Coord.X=0;Coord.Y=0;})
            |>(fun (b, _) -> board <- b)
            |> ignore)
    |> ignore

    let closest = board
                    |> Map.filter (fun coord wires -> wires.Length = 2 && coord <> {X=0;Y=0;})
                    |> Map.map (fun coord lst -> coord)
                    |> Map.toSeq
                    |> Seq.map fst
                    |> Seq.map distanceFromOrigin
                    |> Seq.min
    stopwatch.Stop()
    printfn "Part 1: %i\nCalculation took %i ms" closest stopwatch.ElapsedMilliseconds
    0 // return an integer exit code
