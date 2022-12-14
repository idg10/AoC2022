open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let testInput = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

let input = getEmbeddedInput ()

let pXY<'a> : Parser<(int * int), 'a> =
    pipe2
        (pint32 .>> pchar ',')
        pint32
        (fun x y -> (x, y))

type ScanLine = ScanLine of (int * int) list
let pScanLine<'a> : Parser<ScanLine, 'a> =
    sepBy1 pXY (pstring " -> ") |>> ScanLine

let pScanLines<'a> : Parser<ScanLine list, 'a> = sepBy1 pScanLine newline

let testScanLines = testp pScanLines testInput
let scanLines = testp pScanLines input

testScanLines[0] =! ScanLine [(498,4); (498,6); (496,6)]
testScanLines[1] =! ScanLine [(503,4); (502,4); (502,9); (494,9)]

type ScanDimensions = ScanDimensions of minX:int * min:int * maxY:int *  max:int

let getDimensions lines =
    lines
    |> Seq.collect (fun (ScanLine x) -> x)
    |> Seq.fold
        (fun (minX, minY, maxX, maxY) (x, y) ->
            (min x minX, min y minY, max x maxX, max y maxY))
        (System.Int32.MaxValue, 0, System.Int32.MinValue, System.Int32.MinValue)
    |> ScanDimensions

let testDimensions = getDimensions testScanLines
testDimensions =! ScanDimensions (494, 0, 503, 9)

let cellEmpty = byte 0
let cellRock = byte '#'
let cellSand = byte 'o'

type Grid = Grid of dimensions:ScanDimensions * grid:byte array2d

let makeGrid scanLines =
    let (ScanDimensions (oMinX, oMinY, oMaxX, oMaxY)) = getDimensions scanLines
    // We make the grid wider by 2 to accommodate sand flow
    let (minX, minY, maxX, maxY) = (oMinX - 2, oMinY, oMaxX + 1, oMaxY + 1)
    let dim = ScanDimensions (minX, minY, maxX, maxY)
    let grid = Array2D.create (maxX - minX + 1) (maxY - minY + 1) cellEmpty
    for (ScanLine scanLine) in scanLines do
        let current = None
        for (x, y) in scanLine do
            scanLine
            |> Seq.fold
                (fun lastPosition (nx, ny) ->
                    match lastPosition with
                    | Some (px, py) ->
                        if (nx = px) then
                            // Vertical line
                            let (s, e) = if ny > py then (py, ny) else (ny, py)
                            for y in [s..e] do
                                grid[px - minX, y - minY] <- cellRock
                        else if (ny = py) then
                            // Horizontal line
                            let (s, e) = if nx > px then (px, nx) else (nx, px)
                            for x in [s..e] do
                                grid[x - minX, py - minY] <- cellRock
                        else failwith "Was expecting only horizontal and vertical lines"
                    | None -> ()
                    Some (nx, ny))
                None
            |> ignore
        
    Grid (dim, grid)

// Get value at position in grid
let g (Grid (ScanDimensions (minX, minY, _, _), grid)) x y =
    grid[x - minX, y - minY]

let ug (Grid (ScanDimensions (minX, minY, _, _), grid)) x y value =
    grid[x - minX, y - minY] <- value

let getPath grid =
    let (Grid (ScanDimensions (_, _, _, maxY), _)) = grid
    Seq.unfold
        (fun (cx, cy) ->
            if cy = (maxY - 1) then None
            else
                let r xy = Some (xy, xy)  // Our state and T are the same. unfold wants both, so dup them
                // A unit of sand always falls down one step if possible
                if (g grid cx (cy + 1)) = cellEmpty then r (cx, cy + 1)
                // attempts to instead move diagonally one step down and to the left
                else if (g grid (cx - 1) (cy + 1)) = cellEmpty then r (cx - 1, cy + 1)
                // attempts to instead move diagonally one step down and to the right
                else if (g grid (cx + 1) (cy + 1)) = cellEmpty then r (cx + 1, cy + 1)
                // If all three possible destinations are blocked, the unit of sand comes to rest 
                else None)
        (500, 0)

let addOneDropToGrid grid =
    match (getPath grid |> Seq.tryLast) with
    | None -> None
    | Some (fx, fy) ->
        let (Grid (ScanDimensions (minX, minY, _, _), gridArray)) = grid
        ug grid fx fy cellSand
        Some (fx, fy)

let sb = new System.Text.StringBuilder()
let printGrid grid =
    let (Grid (ScanDimensions (minX, minY, maxX, maxY), _)) = grid
    sb.Clear() |> ignore
    for y in [minY..maxY] do
        for x in [minX..maxX] do
            let c = g grid x y
            sb.Append(if c = 0uy then '.' else (char c)) |> ignore
        sb.AppendLine() |> ignore
    System.Console.WriteLine(sb)

let runGrid grid consoleY printerval =
    let r =
        let (Grid (ScanDimensions (_,_,_, maxY), _)) = grid
        Seq.unfold
            (fun i ->
                match (addOneDropToGrid grid) with
                | None -> None
                | Some (_, fy) -> 
                    System.Console.CursorTop <- consoleY
                    if (i % printerval) = 0 then printGrid grid
                    if fy = (maxY - 1) then None else Some (i+1, i+1))
            (0)
        |> Seq.last
    // Print one last time so we see the final state even when skipping some for speed
    System.Console.CursorTop <- consoleY
    printGrid grid
    r
    
let testGrid = makeGrid testScanLines
let inputGrid = makeGrid scanLines
runGrid testGrid 1 1 =! 24

printf "Part 1: %d\n" (runGrid inputGrid 20 1)

let makeGridPart2 scanLines =
    let (ScanDimensions (oMinX, oMinY, oMaxX, oMaxY)) = getDimensions scanLines
    let height = oMaxY - oMinY
    // We make the grid wider by the total height in both directions to accommodate sand flow
    let (minX, minY, maxX, maxY) = (oMinX - height - 2, oMinY, oMaxX + height + 1, oMaxY + 3)
    let dim = ScanDimensions (minX, minY, maxX, maxY)
    let grid = Array2D.create (maxX - minX + 1) (maxY - minY + 1) cellEmpty
    for (ScanLine scanLine) in scanLines do
        let current = None
        for (x, y) in scanLine do
            scanLine
            |> Seq.fold
                (fun lastPosition (nx, ny) ->
                    match lastPosition with
                    | Some (px, py) ->
                        if (nx = px) then
                            // Vertical line
                            let (s, e) = if ny > py then (py, ny) else (ny, py)
                            for y in [s..e] do
                                grid[px - minX, y - minY] <- cellRock
                        else if (ny = py) then
                            // Horizontal line
                            let (s, e) = if nx > px then (px, nx) else (nx, px)
                            for x in [s..e] do
                                grid[x - minX, py - minY] <- cellRock
                        else failwith "Was expecting only horizontal and vertical lines"
                    | None -> ()
                    Some (nx, ny))
                None
            |> ignore
    for x in [minX..maxX] do
        grid[x - minX, (maxY - minY) - 1] <- cellRock
    Grid (dim, grid)

let testGrid2 = makeGridPart2 testScanLines
let inputGrid2 = makeGridPart2 scanLines
(runGrid testGrid2 1 1) + 1 =! 93

printf "Part 2: %d\n" ((runGrid inputGrid2 20 100) + 1)
