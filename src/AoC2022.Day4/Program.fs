open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let testInput = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

let testLines = testInput |> splitIntoRows
let inputLines = getEmbeddedRows ()

// Typed model for our inputs
type SectionRange = SectionRange of first:int * last:int
type SectionAssignment = SectionAssignment of first:SectionRange * second:SectionRange

// Input parsing
// First, an individual range (e.g. "2-3" or "6-6")
let pSectionRange<'a> : Parser<SectionRange, 'a> =
    pipe2
        pint32
        (pstring "-" >>. pint32)
        (fun first last-> SectionRange (first, last))

testp pSectionRange "3-9" =! SectionRange (3, 9)

// Now a whole line, which is a comma-separated pair of ranges
// (e.g. "2-3,4-5")
let pInputLine<'a> : Parser<SectionAssignment, 'a> =
    pipe2
        pSectionRange
        (pstring "," >>. pSectionRange)
        (fun first second -> SectionAssignment (first, second))

let parseInputLine s = testp pInputLine s
let parseInput rows = Seq.map parseInputLine rows

let testAssignments = parseInput testLines |> List.ofSeq
let inputAssignments = parseInput inputLines  |> List.ofSeq

// Test that our parsing works.
testAssignments.[0] =! SectionAssignment ((SectionRange (2, 4)), SectionRange (6, 8))
testAssignments.[1] =! SectionAssignment ((SectionRange (2, 3)), SectionRange (4, 5))
testAssignments.[2] =! SectionAssignment ((SectionRange (5, 7)), SectionRange (7, 9))
testAssignments.[3] =! SectionAssignment ((SectionRange (2, 8)), SectionRange (3, 7))
testAssignments.[4] =! SectionAssignment ((SectionRange (6, 6)), SectionRange (4, 6))
testAssignments.[5] =! SectionAssignment ((SectionRange (2, 6)), SectionRange (4, 8))

// The various possible styles of overlap
type Overlap =
    // 12.... or ....4.
    // ...45.    12....
    NoOverlap

    // 12... or ..34.
    // 12...    ..34.
    | ExactOverlap

    // 1234. or 1234. or .234.
    // .23..    123..    ...4.
    | FirstFullyContainsSecond

    // .23.. or 123.. or ...4.
    // 1234.    1234.    .234.
    | SecondFullyContainsFirst

    // 123..
    // .234.
    | PartialOverlapFirstThenSecond

    // .234.
    // 123..
    | PartialOverlapSecondThenFirst

let getOverlap (SectionRange (r1First, r1Last)) (SectionRange (r2First, r2Last)) =
    if r1First = r2First then
        if r1Last = r2Last then
            ExactOverlap
        else
            if r1Last < r2Last then
                SecondFullyContainsFirst
            else
                FirstFullyContainsSecond
    else if r1First < r2First then
        if r1Last >= r2Last then
            FirstFullyContainsSecond
        else if r1Last >= r2First then
            PartialOverlapFirstThenSecond
        else
            NoOverlap
    else // r1First > r2First
        if r1Last <= r2Last then
            SecondFullyContainsFirst
        else if r1First <= r2Last then
            PartialOverlapSecondThenFirst
        else
            NoOverlap

// 12...
// ..34.
getOverlap (SectionRange (1, 2)) (SectionRange (3, 4)) =! NoOverlap
// 123..
// ..34.
getOverlap (SectionRange (1, 3)) (SectionRange (3, 4)) =! PartialOverlapFirstThenSecond
// 1234.
// ..34.
getOverlap (SectionRange (1, 4)) (SectionRange (3, 4)) =! FirstFullyContainsSecond
// 12345
// ..34.
getOverlap (SectionRange (1, 5)) (SectionRange (3, 4)) =! FirstFullyContainsSecond

// ...3..
// ...34.
getOverlap (SectionRange (3, 3)) (SectionRange (3, 4)) =! SecondFullyContainsFirst
// ...34.
// ...34.
getOverlap (SectionRange (3, 4)) (SectionRange (3, 4)) =! ExactOverlap
// ...345
// ...34.
getOverlap (SectionRange (3, 5)) (SectionRange (3, 4)) =! FirstFullyContainsSecond

// ....4.
// ...34.
getOverlap (SectionRange (4, 4)) (SectionRange (3, 4)) =! SecondFullyContainsFirst
// ....45
// ...34.
getOverlap (SectionRange (4, 5)) (SectionRange (3, 4)) =! PartialOverlapSecondThenFirst

// .....5
// ...34.
getOverlap (SectionRange (5, 5)) (SectionRange (3, 4)) =! NoOverlap


// Examples from our test input
getOverlap (SectionRange (2, 4)) (SectionRange (6, 8)) =! NoOverlap
getOverlap (SectionRange (2, 3)) (SectionRange (4, 5)) =! NoOverlap
getOverlap (SectionRange (5, 7)) (SectionRange (7, 9)) =! PartialOverlapFirstThenSecond
getOverlap (SectionRange (2, 8)) (SectionRange (3, 7)) =! FirstFullyContainsSecond
getOverlap (SectionRange (6, 6)) (SectionRange (4, 6)) =! SecondFullyContainsFirst
getOverlap (SectionRange (2, 6)) (SectionRange (4, 8)) =! PartialOverlapFirstThenSecond

// Our overlap detection is more specific than the problem requires - we distinguish
// between exact matches, the first fully containing the second, and the second fully
// containing the third, but from part 1's perspective at least, all three count as
// one range fully containing the other.
let fullyContains r1 r2 =
    match getOverlap r1 r2 with
    | ExactOverlap | FirstFullyContainsSecond | SecondFullyContainsFirst -> true
    | _ -> false

let assignmentFullyOverlaps (SectionAssignment (r1, r2)) = fullyContains r1 r2

assignmentFullyOverlaps testAssignments.[0] =! false
assignmentFullyOverlaps testAssignments.[1] =! false
assignmentFullyOverlaps testAssignments.[2] =! false
assignmentFullyOverlaps testAssignments.[3] =! true
assignmentFullyOverlaps testAssignments.[4] =! true
assignmentFullyOverlaps testAssignments.[5] =! false

let countContainingOverlaps (assignments:SectionAssignment list) =
    assignments
    |> Seq.filter assignmentFullyOverlaps
    |> Seq.length

countContainingOverlaps testAssignments =! 2

printf "Part 1: %d\n" <| countContainingOverlaps inputAssignments

let overlaps r1 r2 =
    getOverlap r1 r2 <> NoOverlap

let assignmentOverlaps (SectionAssignment (r1, r2)) = overlaps r1 r2

assignmentOverlaps testAssignments.[0] =! false
assignmentOverlaps testAssignments.[1] =! false
assignmentOverlaps testAssignments.[2] =! true
assignmentOverlaps testAssignments.[3] =! true
assignmentOverlaps testAssignments.[4] =! true
assignmentOverlaps testAssignments.[5] =! true

let countOverlaps (assignments:SectionAssignment list) =
    assignments
    |> Seq.filter assignmentOverlaps
    |> Seq.length

countOverlaps testAssignments =! 4

printf "Part 2: %d\n" <| countOverlaps inputAssignments