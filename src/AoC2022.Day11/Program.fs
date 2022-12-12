open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let input = getEmbeddedInput ()

let testInput = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""



type OperationOperand = Old | Value of int
type OperationOperator = Multiply | Add
type Operation = OperationExpression of operator:OperationOperator * rhs:OperationOperand

type MonkeyNotes =
    Monkey of
        id:int *
        startingItems:int64 list *
        operation:Operation *
        testDivisbleBy: int *
        throwToMonkeyIdIfTrue: int *
        throwToMonkeyIdIfFalse: int 

let parseMonkeyId<'a> : Parser<int, 'a> = pstring "Monkey" >>. spaces >>. pint32 .>> pstring ":"

testp parseMonkeyId "Monkey 0:" =! 0
testp parseMonkeyId "Monkey 3:" =! 3

let parseStartingItems<'a> : Parser<int list, 'a> =
    pstring "Starting items: " >>. spaces >>. (sepBy1 pint32 (pstring "," >>. spaces))

testp parseStartingItems "Starting items: 79, 98" =! [79; 98]

let parseOperator<'a> : Parser<OperationOperator, 'a> =
    (pstring "*" >>. preturn Multiply) <|> (pstring "+" >>. preturn Add)

let parseOperand<'a> : Parser<OperationOperand, 'a> =
    (pstring "old" >>. preturn Old) <|> (pint32 |>> Value)

let parseOperation<'a> : Parser<Operation, 'a> =
    pipe2
        (pstring "Operation: new = old" >>. spaces >>. parseOperator)
        (spaces >>. parseOperand)
        (fun operator operand -> OperationExpression (operator, operand))

testp parseOperation "Operation: new = old * 19" =! OperationExpression (Multiply, Value 19)
testp parseOperation "Operation: new = old + 6" =! OperationExpression (Add, Value 6)
testp parseOperation "Operation: new = old * old" =! OperationExpression (Multiply, Old)

let parseDivisibleBy<'a> : Parser<int, 'a> = pstring "Test: divisible by" >>. spaces >>. pint32

testp parseDivisibleBy "Test: divisible by 19" =! 19

let parseActions<'a> : Parser<int * int, 'a> =
    pipe2
        (spaces >>. pstring "If true: throw to monkey" >>. spaces >>. pint32 .>> newline)
        (spaces >>. pstring "If false: throw to monkey" >>. spaces >>. pint32)
        (fun ifTrue ifFalse -> (ifTrue, ifFalse))

let parseMonkey<'a> : Parser<MonkeyNotes, 'a> =
    pipe5
        (parseMonkeyId .>> newline)
        (spaces >>. parseStartingItems .>> newline)
        (spaces >>. parseOperation .>> newline)
        (spaces >>. parseDivisibleBy .>> newline)
        (parseActions)
        (fun id startingItems op divby (ifTrue, ifFalse) ->
            Monkey (id, startingItems |> List.map int64, op, divby, ifTrue, ifFalse))

let parseMonkeys<'a> : Parser<MonkeyNotes list, 'a> =
    sepBy parseMonkey (many1 newline)

let testMonkeys = testp parseMonkeys testInput
let monkeys = testp parseMonkeys input

let rec gcd a b =
    let r = a % b
    if r = (int64 0) then b
    else gcd b r

let lcm a b =
    (a / (gcd a b)) * b

let divisorsCombined =
    Seq.concat [testMonkeys; monkeys]
    |> Seq.fold
        (fun state (Monkey (_,_,_,testDivisibleBy,_,_)) -> lcm state (int64 testDivisibleBy))
        (int64 1)

printf "%A\n" divisorsCombined

testMonkeys[0] =! Monkey (0, [int64 79; int64 98], OperationExpression (Multiply, Value 19), 23, 2, 3)
testMonkeys[1] =! Monkey (1, [int64 54; int64 65; int64 75; int64 74], OperationExpression (Add, Value 6), 19, 2, 0)
testMonkeys[2] =! Monkey (2, [int64 79; int64 60; int64 97], OperationExpression (Multiply, Old), 13, 1, 3)
testMonkeys[3] =! Monkey (3, [int64 74], OperationExpression (Add, Value 3), 17, 0, 1)


let applyOperation (OperationExpression (operator, rhs)) old =
    let operand =
        match rhs with
        | Old -> old
        | Value v -> int64 v
    match operator with
    | Multiply -> operand * old
    | Add -> operand + old

let monkeyOneTurn (divideWorryBy:int) (Monkey (id:int, startingItems, operation, testDivisibleBy, throwToIdIfTrue, throwToIdIfFalse)) =
    match startingItems with
    | currentWorryLevel::remaining ->
        let newWorryLevel = ((applyOperation operation currentWorryLevel) / (int64 divideWorryBy)) % divisorsCombined
        let throwTo =
            if (newWorryLevel % (int64 testDivisibleBy)) = (int64 0) then throwToIdIfTrue
            else throwToIdIfFalse
        Some (throwTo, newWorryLevel, Monkey (id, remaining, operation, testDivisibleBy, throwToIdIfTrue, throwToIdIfFalse))
    | [] -> None

let monkeyTurns divideWorryBy monkey =
    Seq.unfold
        (fun (monkey, moves) ->
            match monkeyOneTurn divideWorryBy monkey with
            | Some (throwTo, newWorryLevel, m) ->
                let r = (m, (throwTo, newWorryLevel)::moves)
                Some (r, r)
            | None -> None)
        (monkey, List.empty)
    |> Seq.map (fun (_, l) -> List.head l)

let updateMonkeysAfterTurn (turnId:int) (turn:(int * int64) seq) (monkeys:MonkeyNotes seq) =
    let newPositionMap = turn |> Seq.groupBy fst |> Seq.map (fun (id, items) -> (id, Seq.map snd items)) |> Map.ofSeq
    monkeys
    |> Seq.map (fun m ->
        //printf "Updating %A\n" m
        let (Monkey (mid, startingItems, operation, testDivisibleBy, throwToIdIfTrue, throwToIdIfFalse)) = m
        let thisMonkeysNewStartingItems =
            if mid = turnId then
                //printf "Monkey %d: not changing because it's this monkey's turn\n" mid
                []
            else
                match newPositionMap.TryFind(mid) with
                | None ->
                    //printf "Monkey %d: no changes\n" mid
                    startingItems
                | Some newItems ->
                   //printf "Monkey %d aquired %A\n" mid newItems
                   List.concat [startingItems; newItems |> List.ofSeq]
        Monkey (mid, thisMonkeysNewStartingItems, operation, testDivisibleBy, throwToIdIfTrue, throwToIdIfFalse))

let testMonkeysAfterMonkey0Turn = List.ofSeq <| updateMonkeysAfterTurn 0 (monkeyTurns 3 testMonkeys[0]) testMonkeys
let getMonkeyStartList (Monkey (_, sl, _,_,_,_)) = sl

//testMonkeysAfterMonkey0Turn[0] |> getMonkeyStartList =! []
//testMonkeysAfterMonkey0Turn[1] |> getMonkeyStartList =! [int64 54; int64 65; int64 75; int64 74]
//testMonkeysAfterMonkey0Turn[2] |> getMonkeyStartList =! [int64 79; int64 60; int64 97]
//testMonkeysAfterMonkey0Turn[3] |> getMonkeyStartList =! [int64 74; int64 500; int64 620]

let getMonkeyId (Monkey (mid, _, _,_,_,_)) = mid

let runOneTurn divideWorryBy monkeys =
    //printf "runOneTurn: %d\n" (Seq.length monkeys)
    monkeys
    |> Seq.fold<MonkeyNotes, MonkeyNotes seq * Map<int, int>>
        (fun (state, inspectionCounts) m ->
            let mid = getMonkeyId m
            // We want the latest state for this monkey, not the original one passed in
            let cm = state |> Seq.find (fun m -> mid = getMonkeyId m)
            let turn = monkeyTurns divideWorryBy cm
            let previousInspectionCount = inspectionCounts.[mid]
            let updatedInspectionCount = previousInspectionCount + (Seq.length turn)
            //printf "Monkey %d turn: %A\n" mid turn
            (
                updateMonkeysAfterTurn mid turn state,
                inspectionCounts.Add(mid, updatedInspectionCount)
            ))
        (monkeys, monkeys |> Seq.map (fun m -> (getMonkeyId m, 0)) |> Map.ofSeq)
    |> fun (m, ic) ->
        (
            m |> List.ofSeq,
            ic |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd |> List.ofSeq
        )

//let testMonkeysAfterTurn1 = runOneTurn 3 testMonkeys |> fst |> List.ofSeq
//let testMonkeyInspectionsAfterTurn1 = runOneTurn 3 testMonkeys |> snd

//testMonkeysAfterTurn1[0] |> getMonkeyStartList =! [20I; 23I; 27I; 26I]
//testMonkeysAfterTurn1[1] |> getMonkeyStartList =! [2080I; 25I; 167I; 207I; 401I; 1046I]
//testMonkeysAfterTurn1[2] |> getMonkeyStartList =! []
//testMonkeysAfterTurn1[3] |> getMonkeyStartList =! []
//testMonkeyInspectionsAfterTurn1 =! [2; 4; 3; 5]

//// We don't get told the inspection counts after the first turn, so we can
//// only check which monkey holds what
//let testMonkeysAfterTurn2 = runOneTurn 3 testMonkeysAfterTurn1 |> fst |> List.ofSeq
//testMonkeysAfterTurn2[0] |> getMonkeyStartList =! [695I; 10I; 71I; 135I; 350I]
//testMonkeysAfterTurn2[1] |> getMonkeyStartList =! [43I; 49I; 58I; 55I; 362I]
//testMonkeysAfterTurn2[2] |> getMonkeyStartList =! []
//testMonkeysAfterTurn2[3] |> getMonkeyStartList =! []

//let testMonkeysAfterTurn3 = runOneTurn 3 testMonkeysAfterTurn2 |> fst |> List.ofSeq
//testMonkeysAfterTurn3[0] |> getMonkeyStartList =! [16; 18; 21; 20; 122]
//testMonkeysAfterTurn3[1] |> getMonkeyStartList =! [1468; 22; 150; 286; 739]
//testMonkeysAfterTurn3[2] |> getMonkeyStartList =! []
//testMonkeysAfterTurn3[3] |> getMonkeyStartList =! []


//let testMonkeysAfterTurn4 = runOneTurn 3 testMonkeysAfterTurn3 |> fst |> List.ofSeq
//testMonkeysAfterTurn4[0] |> getMonkeyStartList =! [491; 9; 52; 97; 248; 34]
//testMonkeysAfterTurn4[1] |> getMonkeyStartList =! [39; 45; 43; 258]
//testMonkeysAfterTurn4[2] |> getMonkeyStartList =! []
//testMonkeysAfterTurn4[3] |> getMonkeyStartList =! []


let getInspectionCounts divideWorryBy monkeys =
    Seq.unfold
        (fun (state, counts) ->
            let (nextState, inspectionCounts) = runOneTurn divideWorryBy state
            let updatedCounts = List.zip counts inspectionCounts |> List.map (fun (x,y) -> x+y)
            Some (updatedCounts, (nextState, updatedCounts)))
        (monkeys, monkeys |> Seq.map (fun _ -> 0) |> List.ofSeq)


let getInspectionCountsAfter divideWorryBy n monkeys =
    getInspectionCounts divideWorryBy monkeys
    |> Seq.skip (n - 1) // To get the first (n = 1) we skip 0, so subtract 1
    |> Seq.head


getInspectionCountsAfter 3 1 testMonkeys =! [2; 4; 3; 5]
getInspectionCountsAfter 3 20 testMonkeys =! [101; 95; 7; 105]

let getMonkeyBusinessPart1 n monkeys =
    getInspectionCountsAfter 3 n monkeys
    |> Seq.ofList
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (fun total n -> total * n) 1

getMonkeyBusinessPart1 20 testMonkeys =! 10605

printf "Part 1: %d\n" (getMonkeyBusinessPart1 20 monkeys)

let getMonkeyBusinessPart2 n monkeys =
    getInspectionCountsAfter 1 n monkeys
    |> Seq.ofList
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (fun total n -> total * (int64 n)) (int64 1)

//getMonkeyBusinessPart2 10000 testMonkeys =! 2713310158I
let sw = new System.Diagnostics.Stopwatch ()
sw.Start()
printf "Part 2: %d\n" (getMonkeyBusinessPart2 10000 monkeys)
sw.Stop()
printf "%A\n\n" sw.Elapsed

for i in [1..10] do
    sw.Restart()
    let _ = (getMonkeyBusinessPart2 (i*1000) monkeys)
    sw.Stop()
    printf "%d: %A\n" (i*1000) sw.Elapsed