open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let testInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
let input = getEmbeddedInput ()

[<StructuredFormatDisplay("{DisplayText}")>]
type RockRow =
    | RockRow of width:int * rockBits:int
    member this.DisplayText = this.ToString()
    override this.ToString() =
        let (RockRow (width, row)) = this
        (sprintf "%B" row).PadLeft(width, '0').Replace('0','.').Replace('1','#')

let makeRockRow bits =
    bits |>
    Seq.fold
        (fun (width, value) bit -> (width + 1, (if bit = 1 then 1 else 0) ||| (value <<< 1)))
        (0, 0)
    |> RockRow

let addNonCollidingBitsToRow (RockRow (existingWidth, existingBits)) (RockRow (newWidth, newBits)) xpos =
    let shiftLeft = existingWidth - newWidth - xpos
    let newBitsInPos = newBits <<< shiftLeft
    if newBitsInPos &&& existingBits <> 0 then failwith "Expected new rock not to overlap with existing rock"
    RockRow (existingWidth, existingBits ||| newBitsInPos)

addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 0 =! (makeRockRow [1;0;0;0;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 1 =! (makeRockRow [0;1;0;0;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 6 =! (makeRockRow [0;0;0;0;0;0;1])

addNonCollidingBitsToRow (makeRockRow [0;0;0;1;0;0;0]) (makeRockRow [1]) 0 =! (makeRockRow [1;0;0;1;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;1;0;0;0]) (makeRockRow [1]) 1 =! (makeRockRow [0;1;0;1;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;1;0;0;0]) (makeRockRow [1]) 6 =! (makeRockRow [0;0;0;1;0;0;1])

addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1;1;1]) 0 =! (makeRockRow [1;1;1;0;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1;1;1]) 4 =! (makeRockRow [0;0;0;0;1;1;1])

addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [0;1;0]) 0 =! (makeRockRow [0;1;0;0;0;0;0])
addNonCollidingBitsToRow (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [0;1;0]) 4 =! (makeRockRow [0;0;0;0;0;1;0])

raises<exn> <@ addNonCollidingBitsToRow (makeRockRow [1;0;0;0;0;0;0]) (makeRockRow [1]) 0 @>

let rockRowsCollide (RockRow (rowWidth, currentRowBits)) (RockRow (rockWidth, newRockBits)) xOffset =
    let bitShiftFromRight = (rowWidth - rockWidth) - xOffset
    if bitShiftFromRight < 0 then true
    else
        (currentRowBits &&& (newRockBits <<< bitShiftFromRight)) <> 0

// SimpleCollision
rockRowsCollide (makeRockRow [0;0;1;0;0]) (makeRockRow [1]) 0 =! false
rockRowsCollide (makeRockRow [0;0;1;0;0]) (makeRockRow [1]) 1 =! false
rockRowsCollide (makeRockRow [0;0;1;0;0]) (makeRockRow [1]) 2 =! true
rockRowsCollide (makeRockRow [0;0;1;0;0]) (makeRockRow [1]) 3 =! false
rockRowsCollide (makeRockRow [0;0;1;0;0]) (makeRockRow [1]) 4 =! false

// RunningOffEnd
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 0 =! false
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 6 =! false
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1]) 7 =! true
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1;1]) 0 =! false
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1;1]) 5 =! false
rockRowsCollide (makeRockRow [0;0;0;0;0;0;0]) (makeRockRow [1;1]) 6 =! true

// Complex
rockRowsCollide (makeRockRow [0;0;1;0;0;0;0]) (makeRockRow [0;1;0]) 0 =! false
rockRowsCollide (makeRockRow [0;0;1;0;0;0;0]) (makeRockRow [0;1;0]) 1 =! true
rockRowsCollide (makeRockRow [0;0;1;0;0;0;0]) (makeRockRow [0;1;0]) 2 =! false

rockRowsCollide (makeRockRow [0;0;1;1;0;0;0]) (makeRockRow [0;1;0]) 0 =! false
rockRowsCollide (makeRockRow [0;0;1;1;0;0;0]) (makeRockRow [0;1;0]) 1 =! true
rockRowsCollide (makeRockRow [0;0;1;1;0;0;0]) (makeRockRow [0;1;0]) 2 =! true
rockRowsCollide (makeRockRow [0;0;1;1;0;0;0]) (makeRockRow [0;1;0]) 3 =! false

rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 0 =! true
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 1 =! true
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 2 =! false
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 3 =! true
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 4 =! true
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 5 =! true
rockRowsCollide (makeRockRow [0;1;0;0;0;1;0]) (makeRockRow [1;1;1]) 6 =! true

[<StructuredFormatDisplay("{DisplayText}")>]
type RockShape =
    | RockShape of width:int * rockRows:RockRow array
    member this.DisplayText = this.ToString()
    override this.ToString() =
        let (RockShape (_, rows)) = this
        let rowStrings = (rows |> Seq.map (fun row -> row.ToString()))
        "\n" + System.String.Join("\n", rowStrings)

let getRockHeight (RockShape (_, rockRows)) = Array.length rockRows

let makeShape rows =
    let height = List.length rows
    let result = Array.create height (makeRockRow [0])
    let mutable maxWidth = 0;
    rows
    |> List.iteri
        (fun y row ->
            result[y] <- makeRockRow row
            let rowWidth = Seq.length row
            maxWidth <- max rowWidth maxWidth)
    RockShape (maxWidth, result)

let rockShapes = [
    makeShape [[1;1;1;1]];
    makeShape [
        [0;1;0];
        [1;1;1];
        [0;1;0];
        ];
    makeShape [
        [0;0;1];
        [0;0;1];
        [1;1;1];
        ]
    makeShape [
        [1];
        [1];
        [1];
        [1]
        ]
    makeShape [
        [1;1];
        [1;1]
        ]
    ]
let repeatingShapes = Seq.initInfinite (fun _ -> rockShapes) |> Seq.collect Seq.ofList

let shapeHorz = rockShapes[0]
let shapePlus = rockShapes[1]
let shapeL = rockShapes[2]
let shapeVert = rockShapes[3]
let shapeSquare = rockShapes[4]

printf "%A\n\n" rockShapes

// The chamber is in effect infinitely tall. We store only the floor and rows with rock in.
[<StructuredFormatDisplay("{DisplayText}")>]
type Chamber =
    | Chamber of width:int * rows:RockRow list
    member this.DisplayText = this.ToString()
    override this.ToString() =
        let (Chamber (_, rows)) = this
        let rowStrings = (rows |> Seq.map (fun row -> row.ToString()))
        System.String.Join("\n", rowStrings)

let createChamber width = Chamber (width, List.singleton (makeRockRow (Array.create width 1)))
let startChamber = createChamber 7
let addRowToChamber row (Chamber (width, existingChamberRows)) =
    Chamber (width, row::existingChamberRows)

let canShapeBeInPosition chamber (RockShape (rockWidth, rockRows)) distanceFromLeft distanceOfShapeTopBelowChamberTop =
    let (Chamber (chamberWidth, chamberRows)) = chamber
    rockRows
    |> Seq.indexed
    |> Seq.forall
        (fun (shapeRowY, shapeRow) ->
            if distanceFromLeft < 0 then false
            else if rockWidth + distanceFromLeft > chamberWidth then false
            else
                let chamberRowIndex = shapeRowY + distanceOfShapeTopBelowChamberTop
                if chamberRowIndex < 0 then true
                else
                    rockRowsCollide chamberRows[chamberRowIndex] shapeRow distanceFromLeft |> not)
    
        
// HitsBottomOfEmptyChamber
canShapeBeInPosition startChamber shapeHorz 0 0 =! false
canShapeBeInPosition startChamber shapeHorz 1 0 =! false
canShapeBeInPosition startChamber shapeHorz 2 0 =! false
canShapeBeInPosition startChamber shapeHorz 3 0 =! false

canShapeBeInPosition startChamber shapePlus 0 -2 =! false
canShapeBeInPosition startChamber shapePlus 1 -2 =! false
canShapeBeInPosition startChamber shapePlus 2 -2 =! false
canShapeBeInPosition startChamber shapePlus 3 -2 =! false
canShapeBeInPosition startChamber shapePlus 4 -2 =! false

// Does not hit when above bottom row of empty chamber
canShapeBeInPosition startChamber shapeHorz 0 -1 =! true
canShapeBeInPosition startChamber shapeHorz 1 -1 =! true
canShapeBeInPosition startChamber shapeHorz 2 -1 =! true
canShapeBeInPosition startChamber shapeHorz 3 -1 =! true

canShapeBeInPosition startChamber shapePlus 0 -3 =! true
canShapeBeInPosition startChamber shapePlus 1 -3 =! true
canShapeBeInPosition startChamber shapePlus 2 -3 =! true
canShapeBeInPosition startChamber shapePlus 3 -3 =! true
canShapeBeInPosition startChamber shapePlus 4 -3 =! true

// Hits right hand edge of empty chamber
canShapeBeInPosition startChamber shapePlus 5 -3 =! false
canShapeBeInPosition startChamber shapeHorz 4 -3 =! false

// Does not quite hit right hand edge of empty chamber
canShapeBeInPosition startChamber shapePlus 4 -3 =! true
canShapeBeInPosition startChamber shapeHorz 3 -3 =! true

// Hits left hand edge of empty chamber
canShapeBeInPosition startChamber shapePlus -1 -5 =! false
canShapeBeInPosition startChamber shapeHorz -1 -5 =! false

// Hits rock on floor of otherwise empty chamber when positions align.
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapePlus 0 -2 =! true
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapePlus 1 -2 =! true
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapePlus 2 -2 =! false
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapePlus 3 -2 =! true
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapePlus 4 -2 =! true

canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapeL 0 -2 =! true
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapeL 1 -2 =! false
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapeL 2 -2 =! false
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapeL 3 -2 =! false
canShapeBeInPosition (addRowToChamber (makeRockRow [0;0;0;1;0;0;0]) startChamber) shapeL 4 -2 =! true

// Each rock appears so that its left edge is two units away from the left wall and its bottom edge
// is three units above the highest rock in the room (or the floor, if there isn't one).

let getStartPositionForRock (RockShape (_, rockRows)) (Chamber _) = (2, -3 - (Array.length rockRows))

getStartPositionForRock shapeHorz startChamber =! (2, -4)
getStartPositionForRock shapePlus startChamber =! (2, -6)
getStartPositionForRock shapeL startChamber =! (2, -6)
getStartPositionForRock shapeVert startChamber =! (2, -7)
getStartPositionForRock shapeSquare startChamber =! (2, -5)


// The jet sequence doesn't restart for each rock. So we need something that can
// represent the current position in the sequence, and a function that gives us the
// next item, and also an updated position.

type SequenceAndPosition = SequenceAndPosition of sequence:char array * position:int
let makeSequence (text:string) = SequenceAndPosition (text.ToCharArray(), 0)

let getNextFromSequence (SequenceAndPosition (sequence, position)) =
    (sequence[position], SequenceAndPosition (sequence, (position + 1) % sequence.Length))

let testSequence = makeSequence testInput
let inputSequence = makeSequence input
let makeRecurringGets seqAndPos =
    Seq.unfold
        (fun state -> Some (getNextFromSequence state))
        seqAndPos

getNextFromSequence testSequence |> fst =! '>'
makeRecurringGets testSequence |> Seq.skip 1 |> Seq.head =! '>'
makeRecurringGets testSequence |> Seq.skip 2 |> Seq.head =! '>'
makeRecurringGets testSequence |> Seq.skip 3 |> Seq.head =! '<'

// Check that it repeats
makeRecurringGets testSequence |> Seq.take (testInput.Length * 2) |> Seq.toArray |> System.String =! (testInput + testInput)

let moveUntilBlocked rockShape chamber sequenceAndPosition =
    let initialPosition = (getStartPositionForRock rockShape chamber)
    Seq.unfold
        (fun ((currX, currY), currSequenceAndPos, nextMoveIsDrop) ->
            if nextMoveIsDrop then
                if canShapeBeInPosition chamber rockShape currX (currY+1) then
                    Some (((currX, currY+1), currSequenceAndPos), ((currX, currY+1), currSequenceAndPos, false))
                else
                    // If we can't drop then we're done
                    None
            else
                let (jetDirection, nextSequenceAndPos) = getNextFromSequence currSequenceAndPos
                let candidateX = if jetDirection = '>' then currX + 1 else currX - 1
                let nextX = if canShapeBeInPosition chamber rockShape candidateX currY then candidateX else currX
                Some (((nextX, currY), nextSequenceAndPos), ((nextX, currY), nextSequenceAndPos, true)))
        (initialPosition, sequenceAndPosition, false)

moveUntilBlocked shapeHorz startChamber testSequence |> Seq.last |> fst =! (2, -1) 

//printf "%A\n" ((moveUntilBlocked shapeHorz startChamber testSequence) |> List.ofSeq)

// New rock could be entirely below existing rock, entirely above it, or split
//  Below      Above     Split
// 0|     # |
//  |    ###|  |  @    |  |   @   |
//  | @   ##|  |  @    |  |   @   |
//  | @    #|  |  @    | 0| # @   |
//  | @    #|  |  @    |  | # @   |
//  | @    #| 0| ####  |  | ###   |
//  +-------+  +-------+  +-------+
//
// So we need to stitch together up to three pieces:
//  Entirely new content above the line
//  The part where the new rock overlaps with existing content
//  The part below the new rock
// But perhaps we could simplify this by generating enough new empty rows
// so that the chamber is always large enough that the rock appears directly
// under the top.

let addRockToChamber rock x y (Chamber (width, currentChamberRows)) =
    let emptyRow = makeRockRow (Seq.init width (fun _ -> 0))
    let rec expandChamber existingRows y =
        if y < 0 then emptyRow::(expandChamber existingRows (y + 1))
        else existingRows
            
    let fullHeightChamberRows = expandChamber currentChamberRows y
    let rockTop = if y < 0 then 0 else y
    let rockHeight = getRockHeight rock
    let (RockShape (_, rockRows)) = rock 
    let rec updateRows shapeRow chamberRows y =
        if y > 0 then
            match chamberRows with
            | [] -> failwith "Unexpectedly got to the end of the chamber before adding the whole rock"
            | chamberRow::remainingChamberRows -> chamberRow::(updateRows shapeRow remainingChamberRows (y-1))
        else
            if shapeRow > rockHeight then chamberRows
            else
                match chamberRows with
                | [] -> failwith "Unexpectedly got to the end of the chamber before adding the whole rock"
                | chamberRow::remainingChamberRows ->
                    if -shapeRow >= rockHeight then
                        chamberRow::remainingChamberRows
                    else
                        let updatedChamberRow = addNonCollidingBitsToRow chamberRow rockRows[-shapeRow] x
                        updatedChamberRow::(updateRows (shapeRow  - 1) remainingChamberRows (y-1))
    //let highestRowBelowRock = rockHeight + y
    let newChamberRows = updateRows 0 fullHeightChamberRows rockTop
    Chamber (width, newChamberRows)

addRockToChamber shapePlus 1 -3 startChamber =! Chamber (7, [
    makeRockRow [0;0;1;0;0;0;0];
    makeRockRow [0;1;1;1;0;0;0];
    makeRockRow [0;0;1;0;0;0;0];
    makeRockRow [1;1;1;1;1;1;1]
    ])

addRockToChamber shapePlus 1 -3 startChamber
    |> addRockToChamber shapePlus 3 -1 =! Chamber (7, [
    makeRockRow [0;0;0;0;1;0;0];
    makeRockRow [0;0;1;1;1;1;0];
    makeRockRow [0;1;1;1;1;0;0];
    makeRockRow [0;0;1;0;0;0;0];
    makeRockRow [1;1;1;1;1;1;1]
    ])

addRockToChamber shapePlus 1 -3 startChamber
    |> addRockToChamber shapePlus 3 -1
    |> addRockToChamber shapeVert 6 0 =! Chamber (7, [
    makeRockRow [0;0;0;0;1;0;1];
    makeRockRow [0;0;1;1;1;1;1];
    makeRockRow [0;1;1;1;1;0;1];
    makeRockRow [0;0;1;0;0;0;1];
    makeRockRow [1;1;1;1;1;1;1]
    ])

let makeChambersFromSequence jetSequence =
    repeatingShapes
    |> Seq.scan
        (fun (chamber, sequenceAndPosition) shape ->
            let ((shapeX, shapeY), newSequenceAndPosition) = moveUntilBlocked shape chamber sequenceAndPosition |> Seq.last 
            let updatedChamber = addRockToChamber shape shapeX shapeY chamber
            (updatedChamber, newSequenceAndPosition))
        (startChamber, jetSequence)
    |> Seq.skip 1   // Don't report the initial empty chamber state

let testChambersAfterEachShape = makeChambersFromSequence testSequence

for (chamber, _) in (testChambersAfterEachShape |> Seq.take 10) do
    printf "%A\n\n" chamber

let getTowerHeight jetSequence steps =
    let (Chamber (_, finalChamberRows), _) =
        makeChambersFromSequence jetSequence
        |> Seq.skip (steps - 1)
        |> Seq.head
    finalChamberRows.Length - 1 // Subtracting one because we don't include the floor

getTowerHeight testSequence 2022 =! 3068

printf "Part 1: %d\n" (getTowerHeight inputSequence 2022)