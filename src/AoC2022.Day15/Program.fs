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

let testSensors = testp pSensors testInput
let input = getEmbeddedInput ()
let sensors = testp pSensors input

testSensors[0] =! Sensor ((2, 18), (-2, 15))
testSensors[1] =! Sensor ((9, 16), (10, 16))
testSensors[13] =! Sensor ((20, 1), (15, 3))

let manhattanDistance (fromX, fromY) (toX, toY) =
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

let makeGrid sensors windowFirst windowLast =
    let dim = getDimensions sensors
    let (ScanDimensions (minX, minY, maxX, maxY)) = dim
    //let grid = Array2D.create (maxX - minX + 1) (maxY - minY + 1) CellEmpty
    let grid =
        sensors |>
        Seq.fold
            (fun (grid:Map<int*int,byte>) (Sensor ((sX, sY), (bX, bY))) ->
                let withSensor =
                    if ((sX >= windowFirst) && (sX <= windowLast)) && ((sY >= windowFirst) && (sY <= windowLast)) then
                        grid.Add((sX - minX, sY - minY), CellSensor)
                    else grid
                if ((bX >= windowFirst) && (bX <= windowLast)) && ((bY >= windowFirst) && (bY <= windowLast)) then
                    grid.Add((bX - minX, bY - minY), CellBeacon)
                else grid)
            Map.empty 
    Grid (dim, windowFirst, windowLast, grid)

// Get value at position in grid
let g (Grid (ScanDimensions (minX, minY, _, _), _, _, grid)) x y =
    match grid.TryFind((x - minX, y - minY)) with
    | Some cell -> cell
    | None -> CellEmpty

// Set value at position in grid
let ug grid x y value =
    let (Grid (dim, windowFirstRow, windowLastRow, map)) = grid
    if (y < windowFirstRow) || (y > windowLastRow) then grid
    else
        let (ScanDimensions (minX, minY, _, _)) = dim
        let newMap = map.Add((x - minX, y - minY), value)
        Grid (dim, windowFirstRow, windowLastRow, newMap)


let sb = new System.Text.StringBuilder()
let printGrid grid =
    let (Grid (ScanDimensions (minX, minY, maxX, maxY), _, _ ,_)) = grid
    sb.Clear() |> ignore
    for y in [minY..maxY] do
        for x in [minX..maxX] do
            let c = g grid x y
            sb.Append(if c = 0uy then '.' else (char c)) |> ignore
        sb.AppendLine() |> ignore
    System.Console.WriteLine(sb)

let updateGridForSensor grid (Sensor ((sX, sY), (bX, bY))) =
    printf "%A\n" (Sensor ((sX, sY), (bX, bY)))
    let distance = manhattanDistance (sX, sY) (bX, bY)
    let (Grid (ScanDimensions (minX, minY, maxX, maxY), windowFirstRow, windowLastRow,_)) = grid
//    [(sY-distance)..(sY+distance)]
    [windowFirstRow..windowLastRow]
    |> Seq.collect (fun y -> [(sX-distance)..(sX+distance)] |> Seq.map (fun x -> (x, y)))
    |> Seq.fold
        (fun grid (x, y) ->
            if (manhattanDistance (sX, sY) (x, y)) <= distance then
                if (g grid x y) = CellEmpty then
                    ug grid x y CellNoBeacon
                else grid
            else grid)
        grid

let testGrid = makeGrid testSensors 0 20
let inputGrid = makeGrid sensors 0 4000000
updateGridForSensor testGrid testSensors[6] |> printGrid

let determineAllKnownNoSensorLocations grid sensors =
    sensors
    |> Seq.fold updateGridForSensor grid

let findPossibleSensorLocation grid =
    let (Grid (ScanDimensions (minX, minY, maxX, maxY), windowFirst, windowLast ,_)) = grid
    [windowFirst..windowLast]
    |> Seq.collect (fun y -> [windowFirst..windowLast] |> Seq.map (fun x -> (x, y)))
    |> Seq.find (fun (x,y) -> (g grid x y) = CellEmpty)

let getDistressTuningFrequency grid =
    let (x, y) = findPossibleSensorLocation grid
    (4000000*x) + y

let populatedTestGrid = determineAllKnownNoSensorLocations testGrid testSensors

printGrid populatedTestGrid
getDistressTuningFrequency populatedTestGrid =! 56000011

let getNonBeaconPositionsForRow grid row =
    let (Grid (ScanDimensions (minX, _, maxX, _), _, _, _)) = grid
    [minX..maxX]
    |> Seq.filter (fun x ->
        match (g grid x row) with
        | CellNoBeacon | CellSensor -> true
        | _ -> false)
    |> Seq.length

printf "\n"
printGrid populatedTestGrid
getNonBeaconPositionsForRow populatedTestGrid 10 =! 26

let populatedGrid = determineAllKnownNoSensorLocations inputGrid sensors

printf "Part 1: %d\n" (getNonBeaconPositionsForRow populatedGrid 2000000)
printf "Part 2: %d\n" (getDistressTuningFrequency populatedGrid)
