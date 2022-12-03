open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let testInput = """A Y
B X
C Z"""

let testLines = testInput |> splitIntoRows
let inputLines = getEmbeddedRows ()

// Typed model of our input
type Play = Rock | Paper | Scissors
type StrategyEntryPart1 = StrategyEntryPart1 of predictedPlay:Play * recommendedPlay:Play
type Outcome = Lose | Draw | Win
type StrategyEntryPart2 = StrategyEntryPart2 of predictedPlay:Play * requiredOutcome:Outcome

// Input parsers
let pPredictedPlay<'a> : Parser<Play, 'a> =
        (stringReturn "A" Rock)
    <|> (stringReturn "B" Paper)
    <|> (stringReturn "C" Scissors)

let pRecommendedPlayPart1<'a> : Parser<Play, 'a> =
        (stringReturn "X" Rock)
    <|> (stringReturn "Y" Paper)
    <|> (stringReturn "Z" Scissors)

let pRecommendedPlayPart2<'a> : Parser<Outcome, 'a> =
        (stringReturn "X" Lose)
    <|> (stringReturn "Y" Draw)
    <|> (stringReturn "Z" Win)

let pInputLinePart1<'a> : Parser<StrategyEntryPart1, 'a> =
    pipe2
        pPredictedPlay
        (spaces >>. pRecommendedPlayPart1)
        (fun predicted recommended -> StrategyEntryPart1 (predicted, recommended))


let pInputLinePart2<'a> : Parser<StrategyEntryPart2, 'a> =
    pipe2
        pPredictedPlay
        (spaces >>. pRecommendedPlayPart2)
        (fun predicted outcome -> StrategyEntryPart2 (predicted, outcome))
    
let parseInputLinePart1 s = testp pInputLinePart1 s
let parseInputPart1 rows = Seq.map parseInputLinePart1 rows
let parseInputLinePart2 s = testp pInputLinePart2 s
let parseInputPart2 rows = Seq.map parseInputLinePart2 rows

let testGuidePart1 = parseInputPart1 testLines |> List.ofSeq
let inputGuidePart1 = parseInputPart1 inputLines  |> List.ofSeq

// Test that our parsing works.
testGuidePart1.[0] =! StrategyEntryPart1 (Rock, Paper)
testGuidePart1.[1] =! StrategyEntryPart1 (Paper, Rock)
testGuidePart1.[2] =! StrategyEntryPart1 (Scissors, Scissors)

let testGuidePart2 = parseInputPart2 testLines |> List.ofSeq
let inputGuidePart2 = parseInputPart2 inputLines  |> List.ofSeq

testGuidePart2.[0] =! StrategyEntryPart2 (Rock, Draw)
testGuidePart2.[1] =! StrategyEntryPart2 (Paper, Lose)
testGuidePart2.[2] =! StrategyEntryPart2 (Scissors, Win)

type RoundOutcome = Player1 | Draw | Player2
let evaluateOutcome (play1:Play) (play2:Play) =
    match play1 with
    | Rock ->
        match play2 with
        | Rock -> Draw
        | Paper -> Player2
        | Scissors -> Player1
    | Paper ->
        match play2 with
        | Rock -> Player1
        | Paper -> Draw
        | Scissors -> Player2
    | Scissors ->
        match play2 with
        | Rock -> Player2
        | Paper -> Player1
        | Scissors -> Draw

evaluateOutcome Rock Paper =! Player2
evaluateOutcome Rock Scissors =! Player1
evaluateOutcome Rock Rock =! Draw

let pickPlayForOutcome (outcome:Outcome) (play1:Play) =
    match outcome with
    | Outcome.Draw -> play1
    | Win ->
        match play1 with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock
    | Lose ->
        match play1 with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper


pickPlayForOutcome Outcome.Draw Rock =! Rock
pickPlayForOutcome Lose Paper =! Rock
pickPlayForOutcome Win Scissors =! Rock

type Round = Round of play1:Play * play2:Play

let makeRoundFromPlay1ForOutcome (outcome:Outcome) (play1:Play) =
    Round (play1, (pickPlayForOutcome outcome play1))

makeRoundFromPlay1ForOutcome Outcome.Draw Rock =! Round (Rock, Rock)
makeRoundFromPlay1ForOutcome Lose Paper =! Round (Paper, Rock)
makeRoundFromPlay1ForOutcome Win Scissors =! Round (Scissors, Rock)

let scoreShape play =
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

scoreShape Paper =! 2

let scoreRound (Round (play1, play2)) =
    let winner = evaluateOutcome play1 play2
    let player1Score =
        (scoreShape play1) +
        match winner with
        | Player1 -> 6
        | Player2 -> 0
        | Draw -> 3
    let player2Score =
        (scoreShape play2) +
        match winner with
        | Player2 -> 6
        | Player1 -> 0
        | Draw -> 3
    (player1Score, player2Score)

scoreRound (Round (Rock, Paper)) =! (1, 8)
scoreRound (Round (Paper, Rock)) =! (8, 1)
scoreRound (Round (Scissors, Scissors)) =! (6, 6)

let totalScorePart1 (guide:StrategyEntryPart1 list) =
    guide
    |> Seq.map (fun (StrategyEntryPart1 (opponent, me)) -> scoreRound (Round (opponent, me)))
    |> Seq.sumBy (fun (opponentScore, myScore) -> myScore)

totalScorePart1 testGuidePart1 =! 15

let totalScorePart2 (guide:StrategyEntryPart2 list) =
    guide
    |> Seq.map (fun (StrategyEntryPart2 (opponent, outcome)) -> makeRoundFromPlay1ForOutcome outcome opponent)
    |> Seq.map scoreRound
    |> Seq.sumBy (fun (opponentScore, myScore) -> myScore)

totalScorePart2 testGuidePart2 =! 12

printf "Part 1: %d\n" (totalScorePart1 inputGuidePart1)
printf "Part 2: %d\n" (totalScorePart2 inputGuidePart2)