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

let rotate2DArray arr =
    let rowCount = Array2D.length1 arr
    let columnCount = Array2D.length2 arr
    [0..(columnCount - 1)]
    |> Seq.map (fun column ->
        [0..(rowCount - 1)]
        |> Seq.map (fun row -> arr[row, column])
        )
    |> array2D

rotate2DArray (array2D [[1;2];[3;4];[5;6]]) =! (array2D [[1;3;5];[2;4;6]])

let visibilityFromTopOrBottom perspective grid =
    let columnCount = Array2D.length2 grid
    [0..(columnCount - 1)]
    |> Seq.map (fun row -> getViewFrom perspective grid row |> visibleForLine)
    |> Seq.map Array.ofSeq
    |> Array.ofSeq
    |> Seq.map (fun a -> if perspective = Bottom then Array.rev a else a)
    |> array2D
    |> rotate2DArray

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

let mergeBoolGrids merge grid1 grid2 =
    let rowCount = Array2D.length1 grid1
    let columnCount = Array2D.length2 grid2
    [0..(rowCount - 1)]
    |> Seq.map (fun row ->
        [0..(columnCount - 1)]
        |> Seq.map (fun column -> merge grid1[row, column] grid2[row, column]))
    |> array2D

let mergeWithOr = mergeBoolGrids (fun x y -> x || y)

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

let countVisible grid =
    // I can't see a way to convert an array2D into a seq of seqs
    let bools = (visibility grid)
    let rowCount = Array2D.length1 bools
    [0..(rowCount - 1)]
    |> Seq.collect (fun row -> bools[row, *])
    |> Seq.filter id
    |> Seq.length

countVisible testGrid =! 21

printf "Part 1: %d\n" (countVisible inputGrid)