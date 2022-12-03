open Swensen.Unquote

open TextHandling

let testInput = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

let testLines = testInput |> splitIntoRows
let inputLines = getEmbeddedRows ()

let getRucksackParts (description: string) =
    let part1 = description.Substring(0, description.Length / 2)
    let part2 = description.Substring(description.Length / 2)
    (Set.ofSeq part1, Set.ofSeq part2)

getRucksackParts "Ab" =! (Set.ofList ['A'], Set.ofList ['b'])

let getCommonParts (description: string) =
    let (part1, part2) = getRucksackParts description
    Set.intersect part1 part2

getCommonParts testLines.[0] =! Set.ofList ['p']
getCommonParts testLines.[1] =! Set.ofList ['L']
getCommonParts testLines.[2] =! Set.ofList ['P']
getCommonParts testLines.[3] =! Set.ofList ['v']
getCommonParts testLines.[4] =! Set.ofList ['t']
getCommonParts testLines.[5] =! Set.ofList ['s']

let getPriority (item:char) =
    match item with
    | _ when item >= 'a' && item <= 'z' -> (int (item - 'a')) + 1
    | _ when item >= 'A' && item <= 'Z' -> (int (item - 'A')) + 27
    | _ -> failwithf "%c cannot be assigned a priority" item

getPriority 'p' =! 16
getPriority 'L' =! 38
getPriority 'P' =! 42
getPriority 'v' =! 22
getPriority 't' =! 20
getPriority 's' =! 19

let sumPriorities lines =
    lines |> Seq.map getCommonParts |> Seq.collect id |> Seq.map getPriority |> Seq.sum
    
sumPriorities testLines =! 157

let getGroupLines (lines: string seq) =
    Seq.chunkBySize 3 lines
    |> List.ofSeq

let testGroups = getGroupLines testLines
let inputGroups = getGroupLines inputLines

// Test that our grouping logic works
testGroups =! [
    [|"vJrwpWtwJgWrhcsFMMfFFhFp";"jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";"PmmdzqPrVvPwwTWBwg"|];
    [|"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";"ttgJtRGJQctTZtZT";"CrZsJsPPZsGzwwsLwLmpwMDw"|];
]

let commonItem (rucksacks:string seq) =
    rucksacks
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Seq.exactlyOne

commonItem testGroups.[0] =! 'r'
commonItem testGroups.[1] =! 'Z'

let getPriorityForCommonItem rucksacks =
    commonItem rucksacks
    |> getPriority

getPriorityForCommonItem testGroups.[0] =! 18
getPriorityForCommonItem testGroups.[1] =! 52

let sumPrioritiesForCommonItems rucksackGroups =
    rucksackGroups
    |> Seq.sumBy getPriorityForCommonItem

testGroups |> Seq.take 2 |> sumPrioritiesForCommonItems =! 70

printf "Part 1: %d\n" (sumPriorities inputLines)
printf "Part 2: %d\n" (sumPrioritiesForCommonItems inputGroups)