﻿open FParsec
open Swensen.Unquote

open TextHandling

let startMarkerDetector candidate =
    (Seq.distinct candidate |> Seq.length) = (Seq.length candidate)


startMarkerDetector "abcd" =! true
startMarkerDetector "aacd" =! false
startMarkerDetector "abad" =! false
startMarkerDetector "abca" =! false
startMarkerDetector "abcb" =! false

let startMarkerScanner (markerSize:int) (isMarker:(char seq) -> bool) items =
    items
    |> Seq.scan
        (fun ((state:char list), _) item ->
            let newState =
                if state.Length = markerSize then
                    item::(List.take (markerSize - 1) state)
                else
                    item::state
            (newState, ((Seq.length newState) = markerSize) && (isMarker newState)))
        ([], false)
    |> Seq.skip 1
    |> Seq.map snd

let fourUniqueScanner = startMarkerScanner 4 startMarkerDetector

fourUniqueScanner "abcd"  |> List.ofSeq =! [false; false; false; true]
fourUniqueScanner "aabcd" |> List.ofSeq  =! [false; false; false; false; true]
fourUniqueScanner "abcdba" |> List.ofSeq =! [false; false; false; true; false; true]

let offsetOfFirstNUnique n input =
    ((startMarkerScanner n startMarkerDetector) input
     |> Seq.takeWhile not
     |> Seq.length) + 1

let offsetOfFirstFourUnique = offsetOfFirstNUnique 4

offsetOfFirstFourUnique "mjqjpqmgbljsphdztnvjfqwrcgsmlb" =! 7
offsetOfFirstFourUnique "bvwbjplbgvbhsrlpgdmjqwftvncz" =! 5
offsetOfFirstFourUnique "nppdvjthqldpwncqszvftbrmjlhg" =! 6
offsetOfFirstFourUnique "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" =! 10
offsetOfFirstFourUnique "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" =! 11


printf "Part 1: %d\n" (offsetOfFirstFourUnique (getEmbeddedInput ()))

let offsetOfFirstFourteenUnique = offsetOfFirstNUnique 14

offsetOfFirstFourteenUnique "mjqjpqmgbljsphdztnvjfqwrcgsmlb" =! 19
offsetOfFirstFourteenUnique "bvwbjplbgvbhsrlpgdmjqwftvncz" =! 23
offsetOfFirstFourteenUnique "nppdvjthqldpwncqszvftbrmjlhg" =! 23
offsetOfFirstFourteenUnique "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" =! 29
offsetOfFirstFourteenUnique "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" =! 26

printf "Part 2: %d\n" (offsetOfFirstFourteenUnique (getEmbeddedInput ()))
