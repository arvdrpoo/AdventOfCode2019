// Learn more about F# at http://fsharp.org

let calcFuel mass =
    (mass/3)-2

let rec calcFuelRec mass soFar =
    let neededMass = calcFuel mass
    if neededMass > 0 then
        calcFuelRec neededMass (soFar + neededMass)
    else soFar

let calcFuel2 mass =
    calcFuelRec mass 0

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    // read and parse input
    let input = Util.Base.readLines "input.txt"
                |> Seq.map (fun a -> a() |> int)
    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    // part 1
    let res1 = input
            |> Seq.map calcFuel
            |> Util.Arr.tee
            |> Seq.sum

    stopwatch.Stop()

    printfn "Solution part 1: %i.\nCalculation took %i ms" res1 stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    // part 2
    let res2 = input
            |> Seq.map calcFuel2
            |> Util.Arr.tee
            |> Seq.sum

    stopwatch.Stop()
    printfn "Solution part 2: %i.\nCalculation took %i ms" res2 stopwatch.ElapsedMilliseconds

    0 // return an integer exit code
