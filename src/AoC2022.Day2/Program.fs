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
type StrategyEntry = StrategyEntry of predictedPlay:Play * recommendedPlay:Play

// Input parsers
let pPredictedPlay<'a> : Parser<Play, 'a> =
        (stringReturn "A" Rock)
    <|> (stringReturn "B" Paper)
    <|> (stringReturn "C" Scissors)

let pRecommendedPlay<'a> : Parser<Play, 'a> =
        (stringReturn "X" Rock)
    <|> (stringReturn "Y" Paper)
    <|> (stringReturn "Z" Scissors)

let pInputLine<'a> : Parser<StrategyEntry, 'a> =
    pipe2
        pPredictedPlay
        (spaces >>. pRecommendedPlay)
        (fun predicted recommended -> StrategyEntry (predicted, recommended))

    
let parseInputLine s = testp pInputLine s
let parseInput rows = Seq.map parseInputLine rows

let testGuide = parseInput testLines |> List.ofSeq
let inputGuide = parseInput inputLines  |> List.ofSeq


// Test that our parsing works.
testGuide.[0] =! StrategyEntry (Rock, Paper)
testGuide.[1] =! StrategyEntry (Paper, Rock)
testGuide.[2] =! StrategyEntry (Scissors, Scissors)


type Outcome = Player1 | Draw | Player2
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

type Round = Round of play1:Play * play2:Play

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

let totalScore (guide:StrategyEntry list) =
    guide
    |> Seq.map (fun (StrategyEntry (opponent, me)) -> scoreRound (Round (opponent, me)))
    |> Seq.sumBy (fun (opponentScore, myScore) -> myScore)

totalScore testGuide =! 15

printf "Part 1: %d" (totalScore inputGuide)