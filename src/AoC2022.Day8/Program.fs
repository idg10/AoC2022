open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let testInput = """30373
25512
65332
33549
35390"""

let testLines = testInput |> splitIntoRows
let inputLines = getEmbeddedRows ()

let parseLine line =
    line
    |> Seq.map (fun c -> string c |> System.Int32.Parse)
    |> List.ofSeq

(parseLine "30373")[0] =! 3
(parseLine "30373")[1] =! 0
(parseLine "30373")[2] =! 3
(parseLine "30373")[3] =! 7
(parseLine "30373")[4] =! 3

let parseInput lines =
    lines
    |> Seq.map parseLine
    |> array2D

let testGrid = parseInput testLines
let inputGrid = parseInput inputLines

Array2D.length1 testGrid =! 5
Array2D.length2 testGrid =! 5

// It's [row, column]
testGrid[0, 0] =! 3
testGrid[0, 1] =! 0
testGrid[0, 2] =! 3
testGrid[0, 3] =! 7
testGrid[0, 4] =! 3

testGrid[1, 0] =! 2
testGrid[2, 0] =! 6
testGrid[3, 0] =! 3
testGrid[4, 0] =! 3

testGrid[4, 4] =! 0


type ViewFrom = Top | Left | Bottom | Right

let getViewFrom perspective grid position =
    let rowCount = Array2D.length1 grid
    let columnCount = Array2D.length2 grid
    let (y0, dy, x0, dx, count) =
        match perspective with
        | Top -> (position, 0, 0, 1, columnCount)
        | Left -> (0, 1, position, 0, rowCount)
        | Bottom -> (position, 0, rowCount - 1, -1, columnCount)
        | Right -> (columnCount - 1, -1, position, 0, rowCount)
    [1..count]
    |> Seq.scan
        (fun ((x, y), _) _ -> ((x + dx, y + dy), grid[x, y]))
        ((x0, y0), 0)
    |> Seq.skip 1
    |> Seq.map snd
    |> List.ofSeq

getViewFrom Left testGrid 0 =! [3;0;3;7;3]
getViewFrom Left testGrid 1 =! [2;5;5;1;2]
getViewFrom Top testGrid 0 =! [3;2;6;3;3]
getViewFrom Top testGrid 1 =! [0;5;5;3;5]
getViewFrom Right testGrid 0 =! [3;7;3;0;3]
getViewFrom Right testGrid 1 =! [2;1;5;5;2]
getViewFrom Bottom testGrid 0 =! [3;3;6;2;3]
getViewFrom Bottom testGrid 1 =! [5;3;5;5;0]

let visibleForLine heights =
    heights
    |> Seq.scan
        (fun (highestYet, _) height ->
            if height > highestYet then (height, true)
            else (highestYet, false))
        (-1, false)
    |> Seq.skip 1
    |> Seq.map snd
    |> List.ofSeq

// Instructions describe visibility of the '5' at [1,1] from all angles.
// From Left (along row left to right), yes:
visibleForLine (getViewFrom Left testGrid 1) =! [true;true;false;false;false]
// From Right (along row right to left), no:
visibleForLine (getViewFrom Right testGrid 1) =! [true;false;true;false;false]
// From Top (along column top to bottom): yes:
visibleForLine (getViewFrom Top testGrid 1) =! [true;true;false;false;false]
// From Bottom (along column bottom to top): no:
visibleForLine (getViewFrom Bottom testGrid 1) =! [true;false;false;false;false]

visibleForLine (getViewFrom Left testGrid 0) =! [true;false;false;true;false]

let visibilityFromLeftOrRight perspective grid =
    let rowCount = Array2D.length1 grid
    [0..(rowCount - 1)]
    |> Seq.map (fun row -> getViewFrom perspective grid row |> visibleForLine)
    |> Seq.map Array.ofSeq
    |> Seq.map (fun a -> if perspective = Right then Array.rev a else a)
    |> Array.ofSeq
    |> array2D

let flip2DArrayAlongDiagonal arr =
    let rowCount = Array2D.length1 arr
    let columnCount = Array2D.length2 arr
    [0..(columnCount - 1)]
    |> Seq.map (fun column ->
        [0..(rowCount - 1)]
        |> Seq.map (fun row -> arr[row, column])
        )
    |> array2D

flip2DArrayAlongDiagonal (array2D [[1;2];[3;4];[5;6]]) =! (array2D [[1;3;5];[2;4;6]])

let rotate2DArrayClockwise arr =
    let rowCount = Array2D.length1 arr
    let columnCount = Array2D.length2 arr
    [0..(columnCount - 1)]
    |> Seq.map (fun column ->
        [1..(rowCount)]
        |> Seq.map (fun row -> arr[rowCount - row, column])
        )
    |> array2D

rotate2DArrayClockwise (array2D [[1;2];[3;4]]) =! (array2D [[3;1];[4;2]])


let visibilityFromTopOrBottom perspective grid =
    let columnCount = Array2D.length2 grid
    [0..(columnCount - 1)]
    |> Seq.map (fun row -> getViewFrom perspective grid row |> visibleForLine)
    |> Seq.map Array.ofSeq
    |> Array.ofSeq
    |> Seq.map (fun a -> if perspective = Bottom then Array.rev a else a)
    |> array2D
    |> flip2DArrayAlongDiagonal

let visibilityFromLeft = visibilityFromLeftOrRight Left
let visibilityFromRight = visibilityFromLeftOrRight Right
let visibilityFromTop = visibilityFromTopOrBottom Top
let visibilityFromBottom = visibilityFromTopOrBottom Bottom

let printGrid mapper grid =
    let rowCount = Array2D.length1 grid
    let columnCount = Array2D.length2 grid
    for row in [0..(rowCount - 1)] do
        for column in [0..(columnCount - 1)] do
            printf "%c" (mapper grid[row, column])
        printf "\n"

let printBoolGrid = printGrid (fun b -> if b then 'T' else 'F')
//printBoolGrid (visibilityFromLeft testGrid)
//printf "\n"
//printBoolGrid (visibilityFromRight testGrid)
//printf "\n"
//printBoolGrid (visibilityFromTop testGrid)
//printf "\n"
//printBoolGrid (visibilityFromBottom testGrid)
//printf "\n"

let mergeGrids merge grid1 grid2 =
    let rowCount = Array2D.length1 grid1
    let columnCount = Array2D.length2 grid2
    [0..(rowCount - 1)]
    |> Seq.map (fun row ->
        [0..(columnCount - 1)]
        |> Seq.map (fun column -> merge grid1[row, column] grid2[row, column]))
    |> array2D

let mergeWithOr = mergeGrids (fun x y -> x || y)

mergeWithOr (array2D [[false;false];[true;true]]) (array2D [[false;true];[false;true]]) =! array2D [[false;true];[true;true]]
mergeWithOr
    (array2D [[true;false;false;true;false;];[true;true;false;false;false;];[true;false;false;false;false;];[true;false;true;false;true;];[true;true;false;true;false;]])
    (array2D [[false;false;false;true;true;];[false;false;true;false;true;];[true;true;false;true;true;];[false;false;false;false;true;];[false;false;false;true;true;]])
    =! 
    (array2D [[true;false;false;true;true;];[true;true;true;false;true;];[true;true;false;true;true;];[true;false;true;false;true];[true;true;false;true;true;]])

let visibility grid =
    [visibilityFromRight; visibilityFromTop; visibilityFromBottom]
    |> Seq.fold
        (fun v m -> mergeWithOr v (m grid))
        (visibilityFromLeft grid)

//printBoolGrid (visibility testGrid)

let array2dAsSeq arr =
    // I can't see a way to convert an array2D into a seq of seqs
    let rowCount = Array2D.length1 arr
    [0..(rowCount - 1)]
    |> Seq.collect (fun row -> arr[row, *])

let countVisible grid =
    let bools = (visibility grid)
    array2dAsSeq bools
    |> Seq.filter id
    |> Seq.length

countVisible testGrid =! 21

printf "Part 1: %d\n" (countVisible inputGrid)

let cumulativeVisibility (heights:int seq) =
    heights
    |> Seq.scan
        (fun (distanceTo:(int -> int), maxHeight, _) height ->
            let distance = distanceTo height
            (fun h ->
                if h <= height then
                    // We are as tall as or taller than a subsequent tree looking in our
                    // direction, so the buck stops here
                    1
                else
                    // We are not in the way, so pass the buck
                    1 + (distanceTo h)),
            0,
            distance)

            // This logic turned out to be wrong - it assumed that in cases like this:
            // 33549
            // that 9 wouldn't be able to see over the top of the 5 to the 3s. But apparently
            // it can. (It's a bit unclear, because in real life you'd need to work out angles.
            // Probably one of the 3s would be visible but the other wouldn't.) The examples
            // given in the instructions didn't resolve this, so I initially assumed that
            // a tree would block any lower trees behind it. E.g.
            //                (H)
            //    ^            ^
            //    ^            ^
            //    ^            ^
            //    ^            ^
            //    ^            ^
            //    ^            ^
            //    ^            ^
            //    ^            ^
            // ^  ^            ^
            // 1  2            3
            //
            // The question is can the treehouse at (H) (on top of 3) see the tree at 1. It
            // looks pretty obvious that it can't, but according to the puzzle logic, it
            // can.
            // The following code implemented my assumption that 1 would be invisible to (H).
            //if height > maxHeight then
            //    // This is the tallest tree. It blocks out everything behind it, so our
            //    // distance function becomes simple - this will be the only visible tree.
            //    ((fun _ -> 1), height, distance)
            //else
            //    // There are taller trees behind this one, so for subsequent trees taller
            //    // than us, it's as though we're not here.
            //    ((fun h -> if h > height then (distanceTo h) + 1 else 1), maxHeight, distance))
        (
            // The first part of the state is a function which returns the distance you can see from
            // a particular height. Initially this is always zero. As we walk along, we modify this
            // function.
            (fun _ -> 0),
            0,
            0)
    |> Seq.skip 1
    |> Seq.map (fun (_, _, x) -> x)

cumulativeVisibility [3;0;3;7;3] |> List.ofSeq =! [0;1;2;3;1]

// The instructions tells us about the tree of height 5 in the middle of the 2nd row.
// These cover that (and also visibility for all other trees in the relevant rows and columns)
cumulativeVisibility [2;5;5;1;1] |> List.ofSeq =! [0;1;1;1;1]   // Looking left (accumulate from left to right)
cumulativeVisibility [0;5;5;3;3] |> List.ofSeq =! [0;1;1;1;1]   // Looking up (accumulate from top to bottom)
cumulativeVisibility [1;1;5;5;2] |> List.ofSeq =! [0;1;2;1;1]   // Looking right (accumulate from right to left)
cumulativeVisibility [3;3;5;5;0] |> List.ofSeq =! [0;1;2;1;1]   // Looking down (accumulate from bottom to top)

// They also tell us about the tree of height 5 in the middle of the 4th row
//cumulativeVisibility [3;3;5;4;9] |> List.ofSeq =! [0;1;2;1;2]   // Not clear if that final measure is correct. Can the 9 tree on the right see every single tree? Or are the two 3 trees blocked by the 5? We're assuming the latter.
// Well that produced a wrong answer. Let's try it with the other hypothesis
cumulativeVisibility [3;3;5;4;9] |> List.ofSeq =! [0;1;2;1;4]
cumulativeVisibility [9;4;5;3;3] |> List.ofSeq =! [0;1;2;1;1]

let leftScore grid =
    let rowCount = Array2D.length1 grid
    let columnCount = Array2D.length2 grid
    [0..(rowCount - 1)]
    |> Seq.map (fun row -> cumulativeVisibility grid[row, *])
    |> array2D

let downScore grid = grid |> rotate2DArrayClockwise |> leftScore |> rotate2DArrayClockwise |> rotate2DArrayClockwise |> rotate2DArrayClockwise 
let rightScore grid = grid |> rotate2DArrayClockwise |> rotate2DArrayClockwise |> leftScore |> rotate2DArrayClockwise |> rotate2DArrayClockwise 
let upScore grid = grid |> rotate2DArrayClockwise |> rotate2DArrayClockwise |> rotate2DArrayClockwise |> leftScore |> rotate2DArrayClockwise 

printf "%A\n" (leftScore testGrid)
printf "\n"
printf "%A\n" (upScore testGrid)
printf "\n"
printf "%A\n" (rightScore testGrid)
printf "\n"
printf "%A\n" (downScore testGrid)
printf "\n"

let mergeWithMultiply = mergeGrids (fun x y -> x * y)
let scenicScore grid =
    [upScore; rightScore; downScore]
    |> Seq.fold
        (fun v m -> mergeWithMultiply v (m grid))
        (leftScore grid)

printf "%A\n" (scenicScore testGrid)

let findMaxScenicScore grid =
    scenicScore grid
    |> array2dAsSeq
    |> Seq.max

findMaxScenicScore testGrid =! 8

printf "Part 2: %d\n" (findMaxScenicScore inputGrid)