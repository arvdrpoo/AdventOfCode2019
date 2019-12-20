// Learn more about F# at http://fsharp.org

let calculate opcode arg1 arg2 =
    match opcode with
    | 1 -> arg1 + arg2
    | 2 -> arg1 * arg2
    | _ -> failwithf "unknown opcode %i" opcode

let compute currentPos (inputArr:int[]) =
    let rec compute2 currentPos (inputArr:int[]) runcount =
        match inputArr.[currentPos] with
        | 99 -> (inputArr,runcount)
        | 1 | 2 ->
                calculate inputArr.[currentPos] inputArr.[inputArr.[currentPos+1]] inputArr.[inputArr.[currentPos+2]]
                |> Array.set inputArr inputArr.[currentPos + 3]
                compute2 (currentPos + 4) inputArr (runcount + 1)
        | _ -> failwithf "unknown opcode %i" inputArr.[currentPos]
    compute2 currentPos inputArr 0

let modifyNounVerb memory noun verb =
    let newArr = Array.copy(memory)
    Array.set newArr 1 noun
    Array.set newArr 2 verb
    newArr

let computeFirstElement input noun verb =
    let res,steps = modifyNounVerb input noun verb
                    |> compute 0
    (Array.head res,steps)

let computeInitialFor needed input noun verb =
    let rec computeInitialFor2 needed input noun verb (runcount, totalSteps)=
        let res,steps = (computeFirstElement input noun verb)
        if res = needed then
            (noun, verb, totalSteps + steps, runcount)
        else
            if noun <= 99 && verb <= 99 then
                if verb = 99 then
                    computeInitialFor2 needed input (noun+1) 0 ((runcount+1),steps+totalSteps)
                else
                    computeInitialFor2 needed input noun (verb+1) ((runcount+1),steps+totalSteps)
            else
                (-1,-1,totalSteps + steps, runcount)
    computeInitialFor2 needed input noun verb (0,0)

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
    let computed,steps = computeFirstElement input 12 2
    stopwatch.Stop()

    printfn "Part 1: %i\nCalculation took %i ms and %i steps" computed stopwatch.ElapsedMilliseconds steps

    // part 2
    stopwatch.Restart()
    let noun,verb, steps, runcount =  computeInitialFor 19690720 input 0 0
    let res = 100*(noun) + (verb)
    stopwatch.Stop()
    printfn "Part 2: %i\nCalculation took %i ms and %i runs and %i total steps" res stopwatch.ElapsedMilliseconds runcount steps
    0
