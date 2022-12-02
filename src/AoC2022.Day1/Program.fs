open Swensen.Unquote

open TextHandling

// This takes the lines of input text and adorns them with a number indicating which
// group they are in, where groups are delineated by blank lines.
let groupByBlankLine (rows : string seq) =
    let init:(int * string) = (1, "") in
    let f:(int * string -> string -> int * string) = fun (elfIndex, _) row -> ((if row = "" then elfIndex + 1 else elfIndex), row) in
    rows
    |> Seq.scan f init

let testInput = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

let testRows = testInput |> splitIntoRowsIncludingBlanks
let inputRows = getEmbeddedRowsIncludingBlanks ()

// Test that the input text has been annotated by group number as expected
groupByBlankLine testRows |> List.ofSeq =! [
    (1, ""); (1, "1000"); (1, "2000"); (1, "3000");
    (2, ""); (2, "4000");
    (3, ""); (3, "5000"); (3, "6000");
    (4, ""); (4, "7000"); (4, "8000");  (4, "9000");
    (5, ""); (5, "10000");
    ]

// Typed representation of an Elf's inventry
type ElfInventoryItem = Food of calories:int
type ElfInventory = list<ElfInventoryItem>

// Parses rows of text into a sequence of tuples, one for each group,
// where the first item in the tuple is the group number, and the second
// item is an ElfInventory (a list of ElfInventoryItem).
let convertTextRowsToElfInventories rows =
    rows
    |> groupByBlankLine
    |> Seq.groupBy (fun (elfIndex, _) -> elfIndex)
    |> Seq.map (fun (elfIndex, items) -> (elfIndex, items |> Seq.skip 1 |> Seq.map (fun (_, v) -> Food(int v)) |> List.ofSeq))

convertTextRowsToElfInventories testRows |> List.ofSeq =! [
    (1, [ Food 1000; Food 2000; Food 3000 ]);
    (2, [ Food 4000 ]);
    (3, [ Food 5000; Food 6000 ]);
    (4, [ Food 7000; Food 8000; Food 9000 ]);
    (5, [ Food 10000 ])
]

// Get both the test and real input as maps from elf index to inventory
let testElfGroups = convertTextRowsToElfInventories testRows |> Map.ofSeq
let elfGroups = convertTextRowsToElfInventories inputRows |> Map.ofSeq


// Retrieve the total number of calories of an elf by index.
let getElfTotalCalories (elfIndex: int) (elves: Map<int, ElfInventory>) =
    elves.[elfIndex]
    |> Seq.map (fun (Food calories) -> calories)
    |> Seq.sum

testElfGroups.Count =! 5
testElfGroups.[1] =! [ Food 1000; Food 2000 ; Food 3000 ]
getElfTotalCalories 1 testElfGroups =! 6000
testElfGroups.[2] =! [ Food 4000 ]
getElfTotalCalories 2 testElfGroups =! 4000
testElfGroups.[3] =! [ Food 5000; Food 6000 ]
getElfTotalCalories 3 testElfGroups =! 11000
testElfGroups.[4] =! [ Food 7000; Food 8000 ; Food 9000 ]
getElfTotalCalories 4 testElfGroups =! 24000
testElfGroups.[5] =! [ Food 10000 ]
getElfTotalCalories 5 testElfGroups =! 10000

// Sort the elves by their total carried calorie count, and return their indexes
// (with the indices of the elves carrying the most coming first).
let getIndicesOfElvesByDescendingCalories (elves: Map<int, ElfInventory>) =
    [1..elves.Count]
    |> Seq.map (fun i -> (i, getElfTotalCalories i elves))
    |> Seq.sortByDescending snd
    |> Seq.map fst

// Find the elf carrying the maximum calories. (We used this in part 1,
// but generalising in part 2 meant we no longer used this.)
let getIndexOfElfWithMaxCalories (elves: Map<int, ElfInventory>) =
    getIndicesOfElvesByDescendingCalories elves
    |> Seq.item 0

getIndexOfElfWithMaxCalories testElfGroups =! 4

// Get the total calories carried by each of the elves carrying the most calories.
let getTopElvesCalories (count: int) (elves: Map<int, ElfInventory>) =
    getIndicesOfElvesByDescendingCalories elves
    |> Seq.take count
    |> Seq.map (fun i -> getElfTotalCalories i elves)

getTopElvesCalories 3 testElfGroups |> List.ofSeq =! [ 24000; 11000; 10000 ]

// Get the calories caried in total by the elves carrying the most.
// (This is the goal for part 2. It comes before the solution to part 1,
// because after solving part 2, it became clear we could express it in
// terms of the general solution to part 2.)
let getTopElfTotalCalories (count: int) (elves: Map<int, ElfInventory>) =
    getTopElvesCalories count elves
    |> Seq.sum

getTopElfTotalCalories 3 testElfGroups =! 45000

// Get the number of calories carred by the elf carrying the most calories.
// (This is the goal for part 1.
let getMaxElfCalories (elves: Map<int, ElfInventory>) =
    getTopElfTotalCalories 1 elves

getMaxElfCalories testElfGroups =! 24000


printf "Part 1: %A\n" (getMaxElfCalories elfGroups)
printf "Part 2: %A" (getTopElfTotalCalories 3 elfGroups)