open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let testInput = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

let pXY<'a> : Parser<int * int, 'a> =
    pipe2
        (pstring "x=" >>. pint32 .>> pstring ", ")
        (pstring "y=" >>. pint32)
        (fun x y -> (x, y))

testp pXY "x=-2, y=15" =! (-2, 15)

type Sensor = Sensor of sensorPosition:(int*int) * beaconPosition:(int*int)

let pSensor<'a> : Parser<Sensor, 'a> =
    pipe2
        (pstring "Sensor at " >>. pXY)
        (pstring ": closest beacon is at " >>. pXY)
        (fun sp bp -> Sensor (sp, bp))

let pSensors<'a> : Parser<Sensor list, 'a> = sepBy pSensor newline

let testSensors = testp pSensors testInput |> Array.ofSeq
let input = getEmbeddedInput ()
let sensors = testp pSensors input |> Array.ofSeq

testSensors[0] =! Sensor ((2, 18), (-2, 15))
testSensors[1] =! Sensor ((9, 16), (10, 16))
testSensors[13] =! Sensor ((20, 1), (15, 3))

let inline manhattanDistance (fromX, fromY) (toX, toY) =
    (abs (toX - fromX)) + (abs (toY - fromY))

manhattanDistance (0, 0) (10, 0) =! 10
manhattanDistance (10, 0) (0, 0) =! 10
manhattanDistance (0, 0) (0, 10) =! 10
manhattanDistance (10, 0) (0, 0) =! 10
manhattanDistance (10, 4) (0, 0) =! 14
manhattanDistance (10, 0) (0, 4) =! 14
manhattanDistance (10, 0) (0, 4) =! 14

[<Literal>]
let CellEmpty = 0uy
[<Literal>]
let CellBeacon = 'B'B
[<Literal>]
let CellSensor = 'S'B
[<Literal>]
let CellNoBeacon = '#'B

let populateRowForSensor (Sensor ((sX, sY), (bX, bY))) row xOffset (data:byte array) =
    let arrLen = Array.length data
    let width = arrLen + xOffset
    let distance = manhattanDistance (sX, sY) (bX, bY)
    // Sensors scan over a diamond thanks to the manhattan distancing. We want to
    // work out how wide the slice of that diamond is at this row.
    // If we're on the same row as the sensor, it's the full width. For every row
    // away from the sensor position, it gets shorter by two.
    // Instead of working with the full width, we'll just work out how far the
    // sensor looks to one side, because we avoid the multiplications by two then
    let rowsAwayFromSensor = abs (row - sY)
    //printf "Sensor at %d,%d seeing %d,%d (distance %d) is %d away from this row\n" sX sY bX bY distance rowsAwayFromSensor
    let distanceAtThisRow = distance - rowsAwayFromSensor
    if distanceAtThisRow >= 0 then
        let startX = max xOffset (sX - distanceAtThisRow)
        let endX = min (width-1) (sX + distanceAtThisRow)
        if endX >= startX then Array.fill data (startX - xOffset) (endX - startX + 1) CellNoBeacon
        //for x = startX to endX do
        //    if (data[x - xOffset] = CellEmpty) then
        //        data[x - xOffset] <- CellNoBeacon
    let sXi = sX - xOffset
    let bXi = bX - xOffset
    if (sY = row) && (sXi > 0) && (sXi < arrLen) then
        data[sXi] <- CellSensor
    if (bY = row) && (bXi > 0) && (bXi < arrLen) then
        data[bXi] <- CellBeacon
        
let buildRowForSensors row xOffset (sensors:(Sensor array)) data =
    for i = 0 to (sensors.Length-1) do
        populateRowForSensor sensors[i] row xOffset data

type Range = Range of firstIndex:int * lastIndex:int
type Ranges = Ranges of ranges:Range list

let combineTwoRanges r1 r2 =
    let (Range (first1, last1)) = r1
    let (Range (first2, last2)) = r2
    if first1 <= first2 then
        if last1 < (first2 - 1) then
            // 1111 222
            [r1;r2]
        else if last1 > last2 then
            // 1111111111111
            //      222222
            [r1]
        else
            // 11111111
            //    222222222
            // or
            // 1111111
            //        2222
            [Range (first1, last2)]
    else
        if last2 < (first1 - 1) then
            // 2222 1111
            [r2;r1]
        else if last2 > last1 then
            // 222222222222
            //     1111
            [r2]
        else
            // 2222222
            //   1111111
            // or
            // 2222222
            //        11111
            [Range (first2, last1)]

    //if (last1 < first2) || (last2 < first1) then
    //    [r1;r2]
    //else if first1 < first2 then
    //    (last1 last2 first1 first2) 


combineTwoRanges (Range (1, 10)) (Range (12, 20)) =! [Range (1, 10); Range (12, 20)]
combineTwoRanges (Range (1, 10)) (Range (11, 20)) =! [Range (1, 20)]
combineTwoRanges (Range (1, 10)) (Range (6, 20)) =! [Range (1, 20)]
combineTwoRanges (Range (1, 20)) (Range (6, 7)) =! [Range (1, 20)]
combineTwoRanges (Range (1, 20)) (Range (6, 20)) =! [Range (1, 20)]
combineTwoRanges (Range (1, 19)) (Range (6, 20)) =! [Range (1, 20)]
combineTwoRanges (Range (3, 19)) (Range (1, 20)) =! [Range (1, 20)]
combineTwoRanges (Range (3, 20)) (Range (1, 4)) =! [Range (1, 20)]
combineTwoRanges (Range (3, 20)) (Range (1, 2)) =! [Range (1, 20)]
combineTwoRanges (Range (3, 10)) (Range (1, 1)) =! [ Range (1, 1); Range (3, 10)]

let combineRanges (Ranges ranges) newRange =
    let (Range (newFirst, newLast)) = newRange
    let (inserted, rs) =
        Seq.foldBack
            (fun r (aleadyInserted, list) ->
                if aleadyInserted then
                    (true, r::list)   // Already put the new item in place, so just reassembling the list now
                else
                    let (Range (currFirst, _)) = r
                    if newFirst > currFirst then
                        (true, r::newRange::list)
                    else (false, r::list))
            ranges
            (false, [])
    let rangesWithNewRangeInOrder =
        if inserted then rs
        else newRange::rs // If ranges was empty, or if the new range is before the entire list, the fold won't pick it up

    Seq.unfold
        (fun (processing, remaining) ->
            match processing with
            // Initially, processing will be empty. We want to move things
            // out of input list into the processing list until we have two
            // items.
            | [] ->
                match remaining with
                | [] -> None
                | i::tail -> Some (None, ([i], tail))

            // If there's just one item in the processing list, we want
            // to try to move a second one into the processing list, so that
            // in the next iteration, we can attempt to combine them.
            | p1::[] ->
                match remaining with
                // If there are no more to take, then we just have to move
                // this item into the results
                | [] ->  Some (Some p1, ([], []))
                | i::tail -> Some (None, ([i;p1], tail))

            // If we've got two items in the processing list, we want
            // to attempt to combine them.
            | [p1;p2] ->
                let combined = combineTwoRanges p1 p2
                match combined with
                // If that successfully reduced the two items to one,
                // we want to leave that reduced version on the processing list
                | [p] -> Some (None, ([p], remaining))
                // Reduction wasn't possible. Move the second item into the
                // results, leaving the first one there to see if we can combine
                // it with the next item
                | _ -> Some (Some p2, ([p1], remaining))
            | _ -> failwith "Invalid processing list"
            )
        ([], rangesWithNewRangeInOrder)
    |> Seq.choose id
    |> List.ofSeq
    |> Ranges

combineRanges (Ranges [Range (1, 4); Range (6, 10)]) (Range (3,8)) =! Ranges [Range (1, 10)]
combineRanges (Ranges [Range (1, 4); Range (6, 10)]) (Range (5,8)) =! Ranges [Range (1, 10)]
combineRanges (Ranges [Range (1, 4); Range (6, 10)]) (Range (5,5)) =! Ranges [Range (1, 10)]
combineRanges (Ranges [Range (1, 4); Range (7, 10)]) (Range (6,8)) =! Ranges [Range (1, 4); Range (6, 10)]

let buildRangeForSensor row (Sensor ((sX, sY), (bX, bY))) =
    let distance = manhattanDistance (sX, sY) (bX, bY)
    // Sensors scan over a diamond thanks to the manhattan distancing. We want to
    // work out how wide the slice of that diamond is at this row.
    // If we're on the same row as the sensor, it's the full width. For every row
    // away from the sensor position, it gets shorter by two.
    // Instead of working with the full width, we'll just work out how far the
    // sensor looks to one side, because we avoid the multiplications by two then
    let rowsAwayFromSensor = abs (row - sY)
    //printf "Sensor at %d,%d seeing %d,%d (distance %d) is %d away from this row\n" sX sY bX bY distance rowsAwayFromSensor
    let distanceAtThisRow = distance - rowsAwayFromSensor
    if distanceAtThisRow >= 0 then
        let startX = sX - distanceAtThisRow
        let endX = sX + distanceAtThisRow
        Some (Range (startX, endX))
    else
        None

let buildRangesForSensors row (sensors:(Sensor array)) =
    sensors
    |> Seq.choose (buildRangeForSensor row)
    |> Seq.fold combineRanges (Ranges [])

let sb = new System.Text.StringBuilder()
let printRow (row:(byte array)) =
    sb.Clear() |> ignore
    for c in row do
        sb.Append(if c = 0uy then '.' else (char c)) |> ignore
    System.Console.WriteLine(sb)

type ScanDimensions = ScanDimensions of minX:int * min:int * maxY:int *  max:int
type Grid = Grid of dimensions:ScanDimensions * windowFirst:int * windowLast:int * grid:Map<int*int,byte>

let getDimensions sensors =
    sensors
    |> Seq.fold
        (fun (minX, minY, maxX, maxY) (Sensor ((sX, sY), (bX, bY))) ->
            let distance = manhattanDistance (sX, sY) (bX, bY)
            (min minX (sX-distance), min minY (sY-distance), max maxX (sX+distance), max maxY (sY+distance)))
            //(min minX (min sX bX), min minY (min sY bY), max maxX (max sX bX), max maxY (max sY bY)))
        (System.Int32.MaxValue, 0, System.Int32.MinValue, System.Int32.MinValue)
    |> ScanDimensions

let testDimensions = getDimensions testSensors
testDimensions =! ScanDimensions (-8, -10, 28, 26)  // Without manhattan distance (-2, -2, 25, 22)

let showGrid sensors xOffset =
    let (ScanDimensions (_, _, maxX, maxY)) = getDimensions sensors
    let result = Array.create maxX CellEmpty
    for row in [0..maxY] do
        Array.fill result 0 result.Length CellEmpty
        buildRowForSensors row xOffset sensors result
        printRow result

showGrid testSensors -10
printf "\n"
showGrid testSensors 0

let countNonBeacons (margin:int) row sensors =
    let (ScanDimensions (_, _, maxX, maxY)) = getDimensions sensors
    let rowData = Array.create (maxX + 2 * margin) CellEmpty
    buildRowForSensors row (-margin) sensors rowData
    //printf "countNonBeacons for\n"
    //printRow rowData
    rowData |> Seq.filter (fun c -> c = CellNoBeacon) |> Seq.length
    
countNonBeacons 15 10 testSensors =! 26
printf "\n"
printf "Part 1: %d\n" (countNonBeacons 2000000 2000000 sensors)
printf "\n"

let findPossibleSensorLocation sensors windowSize =
    //let (Grid (ScanDimensions (minX, minY, maxX, maxY), windowFirst, windowLast ,_)) = grid
    let data = Array.create windowSize CellEmpty
    [0..(windowSize-1)]
    |> Seq.choose (fun row ->
        if (row % 100000 = 0) then
            printf "Row: %d\n" row
        let (Ranges ranges) = buildRangesForSensors row sensors
        match ranges with
        // Typically we expect a single range filling the whole window
        | Range (rFirst, rLast)::rest ->
            if (rFirst > 0) then Some (0, row)
            else if (rLast < (windowSize - 1)) then Some (rLast + 1, row)
            else None
        | [] -> failwith "Unexpected complete lack of data!")
    |> Seq.head
        

let getDistressTuningFrequency sensors windowSize =
    let (x, y) = findPossibleSensorLocation sensors windowSize
    (4000000L * (int64 x)) + (int64 y)

getDistressTuningFrequency testSensors 20 =! 56000011
printf "Part 2: %d\n" (getDistressTuningFrequency sensors 4000000)
