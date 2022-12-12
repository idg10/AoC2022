open FParsec
open Swensen.Unquote

open TextHandling


let testInput = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

let testLines = testInput |> splitIntoRows
let inputLines = getEmbeddedRows ()

let processInput lines =
    lines |> Seq.mapi (fun y line ->
        line
        |> Seq.mapi (fun x c ->
            match c with
            | 'S' -> ('a', Some (y, x), None)
            | 'E' -> ('z', None, Some (y, x))
            | _ -> (c, None, None))
        |> (fun annotatedChars ->
            Seq.foldBack
                (fun (c, sIn, eIn) (row, sOut, eOut) -> (c::row, Option.orElse sIn sOut, Option.orElse eIn eOut))
                annotatedChars
                ([], None, None)))
    |> (fun annotatedRows ->
        Seq.foldBack
            (fun (r, sIn, eIn) (grid, sOut, eOut) -> (r::grid, Option.orElse sIn sOut, Option.orElse eIn eOut))
            annotatedRows
            ([], None, None))
    |> (fun result ->
        match result with
        | (heightMap, Some startPos, Some endPos) -> (array2D heightMap, startPos, endPos)
        | _ -> failwith "Didn't find start and end")

let (testHeightMap:char array2d, testStartPos, testEndPos) = processInput testLines
let (heightMap, startPos, endPos) = processInput inputLines

printf "%A\nStart: %A\nEnd: %A\n" testHeightMap testStartPos testEndPos

let makeStartArray (w, h) (sX, sY) =
    let a = Array2D.zeroCreate w h
    a[sX, sY] <- 1
    a

let findNextSteps heightMap shortest =
    let w = Array2D.length1 heightMap
    let h = Array2D.length2 heightMap
    shortest |>
    Array2D.mapi
        (fun x y currentShortest ->
            let height:char = heightMap[x, y]
            if currentShortest > 0 then
                // We already found the shortest path to this point
                currentShortest
            else
                [(-1,0);(1,0);(0,-1);(0,1)]
                |> Seq.map (fun (dx, dy) ->
                    let tx = x + dx
                    let ty = y + dy
                    if tx < 0 || tx >= w || ty < 0 || ty >= h then
                        // Candidate neighbour is off the edge of the map, so that's not viable
                        None
                    else
                        let neighbourShortest = shortest[tx, ty]
                        if neighbourShortest = 0 then
                            // We haven't yet found a path to this neighbour, so we can't determine
                            // the path length to this point via that neighbour
                            None
                        else
                            let neighbourHeight = heightMap[tx, ty]
                            if ((int height) - (int neighbourHeight)) > 1 then
                                // To get from this neighbour to here requires a climb of more than 1, so this is not a path
                                None
                            else
                                Some (neighbourShortest + 1))
                |> Seq.choose id
                |> Seq.sort
                |> Seq.tryHead
                |> Option.defaultValue 0)

let findPaths heightMap startPos =
    Seq.unfold
        (fun shortestSoFar ->
            let nextShortestSteps = findNextSteps heightMap shortestSoFar
            let arraysEqual =
                [0..((Array2D.length1 shortestSoFar) - 1)]
                |> Seq.collect (fun x -> [0..((Array2D.length2 shortestSoFar) - 1)] |> Seq.map (fun y -> (x, y)))
                |> Seq.fold
                    (fun f (x, y) -> f && (shortestSoFar[x,y] = nextShortestSteps[x,y]))
                    true
            if arraysEqual then None
            else Some (nextShortestSteps, nextShortestSteps))
        (makeStartArray ((Array2D.length1 heightMap), Array2D.length2 heightMap) startPos)

//for x in (findPaths testHeightMap testStartPos) do
//    printf "%A\n\n" x

let findDistance heightMap startPos endPos =
    let shortestDistances = findPaths heightMap startPos |> Seq.last
    let (endX, endY) = endPos
    shortestDistances[endX, endY] - 1



findDistance testHeightMap testStartPos testEndPos =! 31

for m in (findPaths heightMap startPos) do
    let sb = new System.Text.StringBuilder()
    printf "%d\n" System.Console.CursorTop
    System.Console.SetCursorPosition(0, 10)
    for y in [0..((Array2D.length1 m) - 1)] do        
        for x in [0..((Array2D.length2 m) - 1)] do
            sb.AppendFormat("{0,4} ", m[y, x]) |> ignore
            //printf "%3d " m[y, x]
        System.Console.WriteLine(sb.ToString())
        sb.Clear() |> ignore
//    for y in [0..((Array2D.length1 m) - 2)] do
    printf "\n"

printf "Part 1: %d\n" (findDistance heightMap startPos endPos)