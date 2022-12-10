open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling


let testInput1 = """noop
addx 3
addx -5"""

let testInput2 = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

let input = getEmbeddedInput ()

type CpuState = Cpu of xRegister:int
type Instruction = Noop | Addx of value:int

let pNoop<'a> : Parser<Instruction, 'a> = pstring "noop" >>. preturn Noop
let pAddx<'a> : Parser<Instruction, 'a> = pstring "addx" >>. spaces >>. pint32 |>> Addx
let pInstruction<'a> : Parser<Instruction, 'a> = pNoop <|> pAddx

testp pInstruction "noop" =! Noop
testp pInstruction "addx 3" =! Addx 3
testp pInstruction "addx -5" =! Addx -5

let pProgram<'a> : Parser<Instruction list, 'a> = sepBy pInstruction newline

let testProgram1 = testp pProgram testInput1
let testProgram2 = testp pProgram testInput2
let program = testp pProgram input

testProgram1[0] =! Noop
testProgram1[1] =! Addx 3
testProgram1[2] =! Addx -5

let ticks instruction =
    match instruction with
    | Noop -> [id]
    | Addx value -> [id; fun (Cpu x) -> Cpu (x + value)]

let runProgram program =
    program
    |> Seq.collect ticks
    |> Seq.scan
        (fun state t -> t state)
        (Cpu 1)

let test1Run = runProgram testProgram1 |> List.ofSeq

test1Run[0] =! Cpu 1
test1Run[1] =! Cpu 1
test1Run[2] =! Cpu 1
test1Run[3] =! Cpu 4
test1Run[4] =! Cpu 4
test1Run[5] =! Cpu -1

let test2Run = runProgram testProgram2 |> List.ofSeq

printf "219: %A\n" test2Run[219]
printf "220: %A\n" test2Run[220]
printf "221: %A\n" test2Run[221]

let dropFinal s =
    s
    |> Seq.scan
        (fun (n, nn) t -> (Some t, n))
        (None, None)
    |> Seq.map snd
    |> Seq.choose id

printf "%A\n" (dropFinal [1;2;3;4;5] |> List.ofSeq)

let part1SignalStrengths program =
    Seq.zip (Seq.initInfinite id) (runProgram program)
    |> Seq.map (fun (tick, Cpu x) -> (tick+1) * x)
    |> dropFinal
    |> Seq.chunkBySize 20
    |> Seq.map Array.last
    |> Seq.chunkBySize 2
    |> Seq.map Array.head
    //|> Seq.take 1

let test2SignalStrengths = part1SignalStrengths testProgram2 |> List.ofSeq

printf "%A\n" test2SignalStrengths

let sumSignalStrengths program = part1SignalStrengths program |> Seq.sum

sumSignalStrengths testProgram2 =! 13140

printf "Part 1: %d\n" (sumSignalStrengths program)

let renderScanline xRegisterValues =
    Seq.zip [0..39] xRegisterValues
    |> Seq.map
        (fun (pos, Cpu xReg) ->
            match (pos - xReg) with
            | -1 | 0 | 1 -> '#'
            | _ -> '.')
    |> fun chars -> (new System.String (Seq.toArray chars))

let renderScreen program =
    let lines =
        runProgram program
        |> Seq.chunkBySize 40
        |> Seq.map renderScanline
    for line in lines do
        printf "%s\n" line
        

renderScreen testProgram2

printf "\n"
renderScreen program
