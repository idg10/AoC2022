open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let testInputText = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

// Note: not splitting into rows this time because the input comes in multiple sections
// and we're going to express that through a top-level parser
let inputText = getEmbeddedInput ()

// Typed model for our inputs
type Crate = Crate of label:char
type Move = Move of quantity:int * fromPosition:int * toPosition:int
type Input = Input of stackCount:int * crateRows:((Crate option list) list) * moves:(Move list)

// Input parsing
// First, an individual crate (e.g. "[D]" or "[N]")
let pCrate<'a> : Parser<Crate, 'a> =
    (pstring "[" >>. anyChar .>> pstring "]")
    |>> Crate

testp pCrate "[D]" =! Crate 'D'
testp pCrate "[N]" =! Crate 'N'

let pCrateOrSpace<'a> : Parser<Crate option, 'a> =
    (pstring "   " |>> fun _ -> None)
    <|>
    (pCrate  |>> Some)

testp pCrateOrSpace "[D]" =! Some (Crate 'D')
testp pCrateOrSpace "[N]" =! Some (Crate 'N')
testp pCrateOrSpace "   " =! None

let pNumberLine<'a> : Parser<int, 'a> =
    spaces >>. many1 (pint32 .>> spaces) |>> List.length

testp pNumberLine " 1   2   3 " =! 3

let pMove<'a> : Parser<Move, 'a> =
    pipe3
        (pstring "move" >>. spaces >>. pint32)
        (spaces >>. pstring "from" >>. spaces >>. pint32)
        (spaces >>. pstring "to" >>. spaces >>. pint32)
        (fun quantity fromPosition toPosition -> Move (quantity, fromPosition, toPosition))

testp pMove "move 1 from 2 to 1" =! Move (1, 2, 1)
testp pMove "move 3 from 1 to 3" =! Move (3, 1, 3)
testp pMove "move 2 from 2 to 1" =! Move (2, 2, 1)
testp pMove "move 1 from 1 to 2" =! Move (1, 1, 2)

let pInput<'a> : Parser<Input, 'a> =
    pipe3
        (many ((sepBy pCrateOrSpace (pstring " ")) .>> newline))
        pNumberLine
        (sepBy pMove newline)
        (fun crates stackCount moves -> Input (stackCount, crates, moves))

let testInput = testp pInput testInputText
let input = testp pInput inputText

testInput =! Input
    (3,
     [
        [None;             Some (Crate 'D'); None];
        [Some (Crate 'N'); Some (Crate 'C'); None];
        [Some (Crate 'Z'); Some (Crate 'M'); Some (Crate 'P')];
     ],
     [
        Move (1, 2, 1);
        Move (3, 1, 3);
        Move (2, 2, 1);
        Move (1, 1, 2);
     ]
    )

type Stacks = Stacks of stacks:(Crate list) list

// For the stacks part - the input comes line by line, but it represents information in columns.
// So we need to rotate the input
let initializeStacks stackCount =
    Stacks ([0..stackCount] |> List.map (fun _ -> List.empty))

let emptyTestStack = initializeStacks 3

let addStackRow (Stacks stacks) (crateRow:((Crate option) list)) =
    Stacks
        (Seq.zip stacks crateRow
         |> Seq.map (fun (stack:(Crate list), crate:(Crate option)) ->
                match crate with
                | None -> stack
                | Some crate -> List.append stack [crate])
         |> List.ofSeq
        )

let ts1 = addStackRow emptyTestStack [None; Some (Crate 'D'); None]
ts1 =! Stacks
    [
        List.empty;
        [Crate 'D'];
        List.empty
    ]
let ts2 = addStackRow ts1 [Some (Crate 'N'); Some (Crate 'C'); None]
ts2 =! Stacks
    [
        [Crate 'N'];
        [Crate 'D'; Crate 'C'];
        List.empty
    ]
let ts3 = addStackRow ts2 [Some (Crate 'Z'); Some (Crate 'M'); Some (Crate 'P')]
ts3 =! Stacks
    [
        [Crate 'N'; Crate 'Z'];
        [Crate 'D'; Crate 'C'; Crate 'M'];
        [Crate 'P']
    ]

let rotateStackCrateDescriptions stackCount crateRows =
    List.fold
        addStackRow
        (initializeStacks stackCount)
        crateRows

let makeStacks input =
    let (Input (stackCount, crateRows, _)) = testp pInput input 
    rotateStackCrateDescriptions stackCount crateRows

let testStacks = makeStacks testInputText
let stacks = makeStacks inputText

testStacks =!  Stacks
    [
        [Crate 'N'; Crate 'Z'];
        [Crate 'D'; Crate 'C'; Crate 'M'];
        [Crate 'P']
    ]

let applyMove (Stacks currentStacks) (Move (quantity, fromIndex, toIndex)) =
    let itemsToMove = currentStacks.[fromIndex - 1] |> List.take quantity |> List.rev
    List.mapi
        (fun index stack ->
            if index = (fromIndex-1) then (List.skip quantity stack)
            else if index = (toIndex-1) then (List.concat [itemsToMove;stack])
            else stack
        )
        currentStacks
    |> Stacks

applyMove (Stacks [[Crate 'A'];[];[]]) (Move (1, 1, 2)) =! (Stacks [[];[Crate 'A'];[]])
// When moving more than one item, the order reverses, because the item that was on top moves first,
// then the item that was underneath that ends up on top in its new position, and so on.
applyMove (Stacks [[Crate 'A'; Crate 'B'; Crate 'C'];[];[]]) (Move (2, 1, 3)) =! (Stacks [[Crate 'C'];[];[Crate 'B'; Crate 'A']])

applyMove testStacks (Move (1, 2, 1)) =! Stacks
    [
        [Crate 'D'; Crate 'N'; Crate 'Z'];
        [Crate 'C'; Crate 'M'];
        [Crate 'P']
    ]

// Same as above, but we don't flip the order, because the crane moves multiple crates at once.
let applyMove2 (Stacks currentStacks) (Move (quantity, fromIndex, toIndex)) =
    let itemsToMove = currentStacks.[fromIndex - 1] |> List.take quantity
    List.mapi
        (fun index stack ->
            if index = (fromIndex-1) then (List.skip quantity stack)
            else if index = (toIndex-1) then (List.concat [itemsToMove;stack])
            else stack
        )
        currentStacks
    |> Stacks

applyMove2 (Stacks [[Crate 'A'];[];[]]) (Move (1, 1, 2)) =! (Stacks [[];[Crate 'A'];[]])
applyMove2 (Stacks [[Crate 'A'; Crate 'B'; Crate 'C'];[];[]]) (Move (2, 1, 3)) =! (Stacks [[Crate 'C'];[];[Crate 'A'; Crate 'B']])

let allMoves stacks moves =
    Seq.scan
        applyMove
        stacks
        moves

let allMoves2 stacks moves =
    Seq.scan
        applyMove2
        stacks
        moves

let allMovesForInput (Input (stackCount, crateRows, moves)) =
    let stacks = rotateStackCrateDescriptions stackCount crateRows
    allMoves stacks moves
let allMoves2ForInput (Input (stackCount, crateRows, moves)) =
    let stacks = rotateStackCrateDescriptions stackCount crateRows
    allMoves2 stacks moves

let testMoves = allMovesForInput testInput |> List.ofSeq

testMoves.[1] =! Stacks
    [
        [Crate 'D'; Crate 'N'; Crate 'Z'];
        [Crate 'C'; Crate 'M'];
        [Crate 'P']
    ]
testMoves.[2] =! Stacks
    [
        [];
        [Crate 'C'; Crate 'M'];
        [Crate 'Z'; Crate 'N'; Crate 'D'; Crate 'P']
    ]
testMoves.[3] =! Stacks
    [
        [Crate 'M'; Crate 'C'];
        [];
        [Crate 'Z'; Crate 'N'; Crate 'D'; Crate 'P']
    ]
testMoves.[4] =! Stacks
    [
        [Crate 'C'];
        [Crate 'M'];
        [Crate 'Z'; Crate 'N'; Crate 'D'; Crate 'P']
    ]

let testMoves2 = allMoves2ForInput testInput |> List.ofSeq

testMoves2.[1] =! Stacks
    [
        [Crate 'D'; Crate 'N'; Crate 'Z'];
        [Crate 'C'; Crate 'M'];
        [Crate 'P']
    ]
testMoves2.[2] =! Stacks
    [
        [];
        [Crate 'C'; Crate 'M'];
        [Crate 'D'; Crate 'N'; Crate 'Z'; Crate 'P']
    ]
testMoves2.[3] =! Stacks
    [
        [Crate 'C'; Crate 'M'];
        [];
        [Crate 'D'; Crate 'N'; Crate 'Z'; Crate 'P']
    ]
testMoves2.[4] =! Stacks
    [
        [Crate 'M'];
        [Crate 'C'];
        [Crate 'D'; Crate 'N'; Crate 'Z'; Crate 'P']
    ]

let getFinalStackStates input =
    let (Stacks stacks) = allMovesForInput input |> Seq.last
    stacks
    |> Seq.map List.head
    |> Seq.map (fun (Crate c) -> string c)
    |> String.concat ""

getFinalStackStates testInput =! "CMZ"

let getFinalStackStates2 input =
    let (Stacks stacks) = allMoves2ForInput input |> Seq.last
    stacks
    |> Seq.map List.head
    |> Seq.map (fun (Crate c) -> string c)
    |> String.concat ""

getFinalStackStates2 testInput =! "MCD"

printf "Part 1: %s\n" (getFinalStackStates input)
printf "Part 2: %s\n" (getFinalStackStates2 input)