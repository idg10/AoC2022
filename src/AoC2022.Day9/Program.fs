open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let sw = new System.Diagnostics.Stopwatch ()

let testInput = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

sw.Start()
let input = getEmbeddedInput ()

type Move =
    Up of d:int
    | Down of d:int
    | Left of d:int
    | Right of d:int

let pMove<'a> : Parser<Move, 'a> =
    (pstring "U" >>. spaces >>. pint32 |>> Up)
    <|>
    (pstring "D" >>. spaces >>. pint32 |>> Down)
    <|>
    (pstring "L" >>. spaces >>. pint32 |>> Left)
    <|>
    (pstring "R" >>. spaces >>. pint32 |>> Right)

testp pMove "U 4" =! Up 4

let pParseInput<'a> : Parser<Move list, 'a> =
    sepBy pMove newline

let testMoves = testp pParseInput testInput
let moves = testp pParseInput input

sw.Stop()
printf "Parse time: %A\n" (sw.Elapsed)

testMoves =! [Right 4; Up 4; Left 3; Down 1; Right 4; Down 1; Left 5; Right 2]

let getRequiredTailMovement (xH, yH) (xT, yT) =
    let (dx, dy) = (xH - xT, yH - yT)
    let moveRequired = ((abs dx) > 1) || ((abs dy) > 1)
    if moveRequired then
        (sign dx, sign dy)
    else
        (0, 0)
    //let dTail d = if (abs d) > 1 then (sign d) else 0
    //(dTail dx, dTail dy)

getRequiredTailMovement (1, 1) (1, 1) =! (0, 0)
getRequiredTailMovement (0, 0) (1, 1) =! (0, 0)
getRequiredTailMovement (0, 1) (1, 1) =! (0, 0)
getRequiredTailMovement (0, 2) (1, 1) =! (0, 0)
getRequiredTailMovement (1, 0) (1, 1) =! (0, 0)
getRequiredTailMovement (1, 2) (1, 1) =! (0, 0)
getRequiredTailMovement (2, 0) (1, 1) =! (0, 0)
getRequiredTailMovement (2, 1) (1, 1) =! (0, 0)
getRequiredTailMovement (2, 2) (1, 1) =! (0, 0)

getRequiredTailMovement (3, 1) (1, 1) =! (1, 0)
getRequiredTailMovement (1, 3) (1, 1) =! (0, 1)
getRequiredTailMovement (2, 1) (1, 3) =! (1, -1)
getRequiredTailMovement (3, 3) (1, 4) =! (1, -1)

let expandMove move =
    let (dx, dy) =
        match move with
        | Left x -> (-x, 0)
        | Right x -> (x, 0)
        | Up y -> (0, -y)
        | Down y -> (0, y)
    Seq.unfold
        (fun (cx, cy) ->
            if cx <> dx || cy <> dy then
                let (mx, my) = ((sign dx), (sign dy))
                Some ((mx, my),(mx + cx, my + cy))
            else
                None)
        (0, 0)

expandMove (Up 4) |> List.ofSeq =! [(0,-1);(0,-1);(0,-1);(0,-1)]

sw.Restart()

let expandMoves moves = Seq.collect expandMove moves
let expandedTestMoves = expandMoves testMoves
let expandedMoves = expandMoves moves

let getHeadPositions moves =
    moves
    |> Seq.scan
        (fun (cx, cy) t -> 
            let (dx, dy) = t
            (cx + dx, cy + dy))
        (0, 0)

//printf "%A\n" (getHeadPositions expandedTestMoves |> List.ofSeq)

let getTailPositions headPositions =
    headPositions
    |> Seq.scan
        (fun (tx, ty) head ->
            let (dx, dy) = getRequiredTailMovement head (tx, ty)
            (tx + dx, ty + dy))
        (0, 0)
    |> Seq.skip 1

//printf "%A\n" (getHeadPositions expandedTestMoves |> getTailPositions |> List.ofSeq)

let countDistinctTailPositions moves =
    getHeadPositions moves
    |> getTailPositions
    |> Seq.distinct
    |> Seq.length

countDistinctTailPositions expandedTestMoves =! 13

printf "Part 1: %d\n" (countDistinctTailPositions expandedMoves)
sw.Stop()
printf "Part 1 time: %A\n" (sw.Elapsed)

let getRopeSegmentPositions segmentCount headPositions =
    [1..segmentCount]
    |> Seq.scan
        (fun state _ -> getTailPositions state)
        headPositions

let getRopeTailPositions segmentCount headPositions =
    getRopeSegmentPositions segmentCount headPositions
    |> Seq.last

let countDistinctRopeTailPositions segmentCount moves =
    getHeadPositions moves
    |> getRopeTailPositions segmentCount
    |> Seq.distinct
    |> Seq.length

countDistinctRopeTailPositions 9 expandedTestMoves =! 1

let testInput2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""

let testMoves2 = testp pParseInput testInput2
let expandedTestMoves2 = expandMoves testMoves2

countDistinctRopeTailPositions 9 expandedTestMoves2 =! 36

sw.Restart()
printf "Part 2: %d\n" (countDistinctRopeTailPositions 9 expandedMoves)
sw.Stop()
printf "Part 2 time: %A\n" (sw.Elapsed)
