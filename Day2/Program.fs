// Learn more about F# at http://fsharp.org

let calculate opcode arg1 arg2 =
    match opcode with
    | 1 -> arg1 + arg2
    | 2 -> arg1 * arg2
    | _ -> failwithf "unknown opcode %i" opcode

let rec compute currentPos (inputArr:int[]) =
    match inputArr.[currentPos] with
    | 99 -> inputArr
    | 1 | 2 ->
            calculate inputArr.[currentPos] inputArr.[inputArr.[currentPos+1]] inputArr.[inputArr.[currentPos+2]]
            |> Array.set inputArr inputArr.[currentPos + 3]
            compute (currentPos + 4) inputArr
    | _ -> failwithf "unknown opcode %i" inputArr.[currentPos]

let modifyNounVerb memory noun verb =
    let newArr = Array.copy(memory)
    Array.set newArr 1 noun
    Array.set newArr 2 verb
    newArr

let computeFirstElement input noun verb =
    modifyNounVerb input noun verb
    |> compute 0
    |> Array.head

let computeInitialFor needed input noun verb =
    let rec computeInitialFor2 needed input noun verb runcount=
        if (computeFirstElement input noun verb) = needed then
            (noun, verb, runcount)
        else
            if noun <= 99 && verb <= 99 then
                if verb = 99 then
                    computeInitialFor2 needed input (noun+1) 0 (runcount+1)
                else
                    computeInitialFor2 needed input noun (verb+1) (runcount+1)
            else
                (-1,-1,runcount)
    computeInitialFor2 needed input noun verb 0

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    // read and parse input
    let input = Util.Base.readLines "input.txt"
                |> Seq.map (fun a -> a())
                |> Seq.head
                |> (fun l -> l.Split(","))
                |> Array.map int

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds

    // part 1
    stopwatch.Restart()

    printfn "\t %i : %A\n" 0 input
    let computed = computeFirstElement input 12 2
    stopwatch.Stop()

    printfn "Part 1: %i\nCalculation took %i ms" computed stopwatch.ElapsedMilliseconds

    // part 2
    stopwatch.Restart()
    let noun,verb, runcount =  computeInitialFor 19690720 input 0 0
    let res = 100*(noun) + (verb)
    stopwatch.Stop()
    printfn "Part 2: %i\nCalculation took %i ms and %i runs" res stopwatch.ElapsedMilliseconds runcount
    0
