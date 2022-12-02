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

let scorePlay play =
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

scorePlay Paper =! 2

type Round = Round of play1:Play * play2:Play

