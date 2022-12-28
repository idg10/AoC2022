open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let testInput = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""
let input = getEmbeddedInput ()

type Valve = Valve of name:string
type ValveSite = ValveSite of valve:Valve * flowRate:int * tunnels:Valve list

let pValveName<'a> : Parser<Valve, 'a> =
    pipe2 anyChar anyChar (fun c1 c2 -> (System.String [|c1;c2|])) |>> Valve

testp pValveName "DD" =! Valve "DD"

let pValveSite<'a> : Parser<ValveSite, 'a> =
    pipe3
        (pstring "Valve " >>. pValveName)
        (pstring " has flow rate=" >>. pint32)
        ((pstring "; tunnels lead to valves " <|>
          pstring "; tunnel leads to valve ") >>. (sepBy1 pValveName (pstring ", ")))
        (fun n r ts -> ValveSite (n, r, ts))

let pValveSites<'a> : Parser<ValveSite list, 'a> = sepBy1 pValveSite newline

let testSites = testp pValveSites testInput
let sites = testp pValveSites input

testSites[0] =! ValveSite (Valve "AA", 0, [Valve "DD"; Valve "II"; Valve "BB"])
testSites[9] =! ValveSite (Valve "JJ", 21, [Valve "II"])

// It's like the travelling salesman problem, except the cost is the same for every trip.
// The Held-Karp algorithm can solve this in O(n²2ⁿ). Our input is of size 57, so that
// 2⁵⁷ is going to destroy us. We must be able to take advantage of the fact that the
// cost of travel is uniform to reduce this to a simpler problem.

// Is it relevant that the example sequence shown is not the optimal path? It solves
// the problem of maximising flow by by opening all the valves that can be opened,
// but in this short example there are several ways to do it. They take 25 minutes (24
// steps) to get there. It's actually possible to do it in 18 minutes (17 steps):
// AA->II->JJ->open->II->AA->BB->open->CC->open->DD->open->EE->open->FF->GG->HH->open
// On the face of it, the example has simply solved a much easier problem, and is
// therefore not of much relevance. But is this a clue? Not sure.

// Could we simplify our map into lobes? With the example problem, we have this sort
// of shape:
//
//  /-->  AA0 <-> II0 <-> JJ21
//  |       ^
//  v       |
//  BB13    |
//  ^       |
//  |       |
//  v       v
//  CC2 -> DD20 <-> EE3 <-> FF0 <-> GG0 <-> HH22
//
// Optimum:
// AA, DD, O, CC, BB, O, AA, II, JJ, O, II, AA, DD, EE, FF, GG, HH, O, GG, FF, EE, O, DD, CC, O
//
// Roughly speaking, there's a 'central' cycle featuring AA, BB, CC, and DD, and then
// we have two simple linear paths from this cluster heading out to JJ and HH.
// I'm calling those things "lobes". The thing that makes them interesting is the limited
// branching. There are basically two things you can do with a lobe:
//  1. go down it and come back up
//  2. go down it and never return
// (It's possible to imagine "terminal lobes" that permit only 2. The simple example
// has none, but it's possible our real input will.) In the simple example, my 18 step
// solution takes option 2 for the longer of the two lobes, which makes sense in a
// hand-wavey fashion: the longer the lobe, the higher the cost of returning from its
// extreme, so it seems plausible that the optimal route might entail leaving the longest
// lobe until last, so that you can avoid the cost of heading down it and back.
//
// So how might we recognize these lobes? We could try to find the termini, and then
// work our way back up. JJ is a terminus because it has only one way in, from II.
// II's only ways in are from the terminus (JJ) or from AA, so if the node that provides
// a way into a terminus has only one other way in, we can say it forms part of a lobe.
//
// So:
//  1. find all termini
//  2. for each terminus, iterate back:
//      find nodes that point to the current node
//      if we found only one, that's part of the lobe, repeat
//      if we found more than one, none of them is part of a lobe

// To do this, we need to be able to ask for any node "what are the routes into this node?"
// So we want a Map<Node, Node>
let getWaysInMap (sites:ValveSite list) =
    sites
    |> Seq.fold
        (fun (m:Map<string, string list>) (ValveSite (Valve vFrom, _, tunnels)) ->
            tunnels
            |> Seq.fold
                (fun m (Valve vTo) ->
                    let routesIn =
                        match (Map.tryFind vTo m) with
                        | Some froms -> vFrom::froms
                        | None -> [vFrom]
                    Map.add vTo routesIn m)
                m)
        Map.empty

let testWaysIn = getWaysInMap testSites
let waysIn = getWaysInMap sites

let findTermini (waysIn:Map<string, string list>) =
    waysIn
    |> Map.toSeq
    |> Seq.filter (fun (_, waysIn) -> (List.length waysIn) = 1)

let testTermini = findTermini testWaysIn
let termini = findTermini waysIn

let findLobeFor terminus (waysIn:Map<string, string list>) =
    Seq.unfold
        (fun (currentValve, whereWeJustCameFrom) ->
            match Map.find currentValve waysIn with
            | [single] -> Some (single, (single, currentValve))
            | [w1; w2] ->
                if w1 = whereWeJustCameFrom then
                    Some (w2, (w2, currentValve))
                else
                   Some (w1, (w1, currentValve))
            | _ -> None)
        (terminus, terminus)

printf "Lobe for JJ %A\n" (findLobeFor "JJ" testWaysIn |> List.ofSeq)
printf "Lobe for HH %A\n" (findLobeFor "HH" testWaysIn |> List.ofSeq)

let findAllLobesForTermini termini waysIn =
    termini
    |> Seq.map (fun (terminusName, _) -> findLobeFor terminusName waysIn |> List.ofSeq)

printf "All test lobes: %A\n" (findAllLobesForTermini testTermini testWaysIn |> List.ofSeq)
printf "All real lobes: %A\n" (findAllLobesForTermini termini waysIn |> List.ofSeq)

// Disappointingly, lobes account for only 12 of the sites.
// More precisely:
// Test: 2 lobes of length 4 and 2 (8 sites)
//  EE FF GG HH
//  II JJ
// Real: 3 lobes of length 3, 6, and 3 (12 sites)
//  DC ST CR
//  EQ ZN HL DW LC JL
//  EX RB CC
// That doesn't pare the problem space down enough.

// What if, instead, we looked for lines (of which lobes are a special case)? Just look for non-branching
// node sequences?
// There might be a relatively small number of clusters joined mostly by lines.

let countNonBranchingLocations (sites:ValveSite list) =
    sites
    |> Seq.filter (fun (ValveSite (valve, _, tunnels)) -> (Seq.length tunnels) <= 2)
    |> Seq.length

printf "Non-branching in test data %d\n" (countNonBranchingLocations testSites)
printf "Non-branching in input data %d\n" (countNonBranchingLocations sites)

// Aha! 47 of my inputs are non-branching.

type SummarizedNetwork =
    SummarizedNetwork
        // Key is (startName, endName), and the value is all the ValveSites in that chain
        of chains:Map<string * string, Set<string>>
        * chainsByStart:Map<string, string*string>
        * complexSites:Map<string, ValveSite>
        * allSize:Map<string, ValveSite>

let summarizeNetwork (sites:ValveSite list) =
    let (SummarizedNetwork (chains, _, complexSites, allSites)) =
        sites
        |> Seq.fold 
            (fun summarizedNetwork valveSite ->
                let addUnidirectionalChainElement fromValve toValve (SummarizedNetwork (chains, chainsByStart, complexSites, allSites)) =
                    let matchingKeys =
                        chains
                        |> Map.keys
                        |> Seq.filter (fun (k1, k2) ->
                            (toValve = k1) || (fromValve = k2))
                        |> List.ofSeq
                    match matchingKeys with
                    | [] ->
                        SummarizedNetwork (
                            chains |> Map.add (fromValve, toValve) (Set.ofList [fromValve; toValve]),
                            chainsByStart |> Map.add fromValve (fromValve, toValve),
                            complexSites,
                            allSites)
                    | keys ->
                        keys
                        |> Seq.fold
                            (fun (SummarizedNetwork (chains, chainsByStart, complexSites, allSites)) (k1, k2) ->
                                let chainMembers = chains[(k1, k2)]
                                let before = k1 = toValve
                                // EE [FF; DD]:
                                //  (EE,FF): EE -> FF; (EE,DD): EE -> DD
                                // FF [EE; GG]
                                //  (EE,GG): EE -> FF -> GG; (FF,DD): FF -> EE -> DD
                                // GG [FF;HH]
                                //  (EE,HH): EE -> FF -> GG -> HH; (GG,DD): GG -> FF -> EE -> DD
                                // HH [GG]
                                //  (EE,HH): EE -> FF -> GG -> HH; (HH,DD): HH -> GG -> FF -> EE -> DD
                            
                                if (before && ((fromValve = k2)
                                    || chainMembers |> Set.contains fromValve))
                                    || ((not before) && ((toValve = k1)
                                            || chainMembers |> Set.contains toValve)) then
                                    // This is pointer back up the chain, so don't try to add this.
                                    SummarizedNetwork (chains, chainsByStart, complexSites, allSites)
                                else
                                    let chainsExceptThis = Map.remove (k1, k2) chains
                                    let chainsByStartExceptThis = Map.remove k1 chainsByStart
                                    let (updatedMap, updatedByStartMap) =
                                        if before then
                                            let updatedChain = chainMembers |> Set.add fromValve
                                            // This site extends the start of the chain
                                            // fromValve..k1/toValve..(chain)..k2
                                            (
                                                chainsExceptThis |> Map.add (fromValve, k2) updatedChain,
                                                chainsByStartExceptThis |> Map.add fromValve (fromValve, k2)
                                            )
                                        else
                                            let updatedChain = chainMembers |> Set.add toValve
                                            // This site extends the end of the chain
                                            // k1..(chain)..k2/fromValve..toValve
                                            (
                                                chainsExceptThis |> Map.add (k1, toValve) updatedChain,
                                                chainsByStartExceptThis |> Map.add k1 (k1, toValve)
                                            )
                                    SummarizedNetwork (updatedMap, updatedByStartMap, complexSites, allSites))
                            (SummarizedNetwork (chains, chainsByStart, complexSites, allSites))

                let (ValveSite (Valve valve, _, _)) = valveSite
                let (SummarizedNetwork (uc, ucbs, cs, all)) =
                    match valveSite with
                    | ValveSite (_, _, [Valve t1; Valve t2]) ->
                        summarizedNetwork
                        |> addUnidirectionalChainElement valve t1
                        |> addUnidirectionalChainElement valve t2
                    | ValveSite (_, _, [Valve t1]) ->
                        addUnidirectionalChainElement valve t1 summarizedNetwork
                    | ValveSite (_, _, _) ->
                        let (SummarizedNetwork (chains, chainsByStart, complexSites, allSites)) = summarizedNetwork
                        SummarizedNetwork (
                            chains,
                            chainsByStart,
                            complexSites |> Map.add valve valveSite,
                            allSites)
                SummarizedNetwork (uc, ucbs, cs, all |> Map.add valve valveSite))
            (SummarizedNetwork (Map.empty, Map.empty, Map.empty, Map.empty))
    let chainsByStart =
        chains
        |> Map.keys
        |> Seq.fold
            (fun m (startValve, endValve) -> m |> Map.add startValve (startValve, endValve))
            Map.empty
    SummarizedNetwork (chains, chainsByStart, complexSites, allSites)

let testSummarized = (summarizeNetwork testSites)
printf "Test summarized: %A\n" testSummarized

let summarized = (summarizeNetwork sites)
printf "Sites summarized: %A\n" summarized

// Test: 2 complex sites and 6 connecting sequences.
// Real: 10 complex sites and 50 connecting sequences.
//
// Bear in mind that we detect connecting sequences in both
// directions, so EE -> HH and HH -> DD are effectively the
// same sequence twice. (They might not look like it. That's
// because DD is part of a compex node, so it only becomes
// a line when you go from DD to EE. But if you're at the HH
// end, EE only has the option to go back to DD.)
// So in practice that's:
// Test: 2 complex sites and 3 bidirectional connecting sequences.
// Read: 10 complex sites and 25 bidirectional connecting sequences.

let calculateBranches (SummarizedNetwork (_, _, complexSites, _)) =
    complexSites.Values
    |> Seq.fold
        (fun total (ValveSite (_, _, tunnels)) ->
            total * (int64 (tunnels.Length)))
        1L
printf "Test branches: %d\n" (calculateBranches testSummarized)
printf "Branches: %d\n" (calculateBranches summarized)

// The test input branches into 9 options.
// The real input branches into 1,800,000 - a lot, but manageable.
// So let's just try that.
// We start at AA, and we're in Complex Node mode.
//  If flow rate non-zero, and not already open, open valve
//      (Question: are there cases where we gain something by not opening a
//       valve so that we have time to make it to a higher-valued valve?)
//  Branch, trying each of the Complex Node's exits.
//  If we're still in a Complex Node:
//      recurse
//  Otherwise:
//      Don't branch. If this is not a terminus:
//          Go to the node we didn't come in from
//      If this is a terminus:
//          Go to the node we just came in from


let findUpperBound valveSites =
    valveSites
    |> List.sumBy (fun (ValveSite (_, flowRate, _)) -> flowRate)

printf "Test max possible: %d\n" (findUpperBound testSites)
printf "Real max possible: %d\n" (findUpperBound sites)

let findMaximumReliefWithChains totalTime network =
    let (SummarizedNetwork (chains, chainsByStart, complexSites, allSites)) = network
    let startSite = complexSites["AA"]
    let rec maxReliefForBranch
            (currentMinute:int)
            (culumativeRelease:int)
            openValves
            currentReleaseRate
            (beenHereBefore:Set<string*string*int>) // From v to v with cumulative release r
            site =
        if currentMinute > totalTime then culumativeRelease
        else
            let (ValveSite (valve, flowRate, exits)) = site
            let (Valve fromValveName) = valve
            let couldOpen = (flowRate > 0) && (not (Set.contains valve openValves))
            exits
            |> Seq.filter (fun (Valve toValve) ->
                not (beenHereBefore |> Set.contains (fromValveName, toValve, currentReleaseRate)))
            |> Seq.map (fun exit ->
                let (Valve toValve) = exit
                maxForValve
                    currentMinute
                    culumativeRelease
                    openValves
                    currentReleaseRate
                    (beenHereBefore |> Set.add (fromValveName, toValve, currentReleaseRate))
                    valve
                    flowRate
                    exit
                    couldOpen)
            |> Seq.append [0] // Sequence will be empty if we already tried all options before
            |> Seq.max
    and maxReliefForLine
        currentMinute
        culumativeRelease
        openValves
        currentReleaseRate
        beenHereBefore
        valve =
        if currentMinute > totalTime then culumativeRelease
        else
            let (_, nextInChain) = chainsByStart[valve]
            let (ValveSite (valve, flowRate, exits)) = allSites[valve]
            let (ValveSite (nextValve, _, _)) = allSites[nextInChain]
            let couldOpen = (flowRate > 0) && (not (Set.contains valve openValves))
            maxForValve
                currentMinute
                culumativeRelease
                openValves
                currentReleaseRate
                beenHereBefore
                valve
                flowRate
                nextValve
                couldOpen
    and maxForValve
        currentMinute
        culumativeRelease
        openValves
        currentReleaseRate
        beenHereBefore
        valve
        flowRate
        (Valve exit)
        couldOpen =
        if currentMinute > totalTime then culumativeRelease
        else
            let maxWithoutOpeningValve =
                match Map.tryFind exit complexSites with
                | Some site ->
                    maxReliefForBranch
                        (currentMinute + 1)
                        culumativeRelease
                        openValves
                        currentReleaseRate
                        beenHereBefore
                        site
                | None ->
                    maxReliefForLine
                        (currentMinute + 1)
                        culumativeRelease
                        openValves
                        currentReleaseRate
                        beenHereBefore
                        exit
            if couldOpen then
                let updatedOpenValves = Set.add valve openValves
                let minutesForWhichValveWillRemainOpen = totalTime - (currentMinute)
                let totalReliefFromThisValve = flowRate * minutesForWhichValveWillRemainOpen
                let updatedCumulativeRelease = culumativeRelease + totalReliefFromThisValve
                let updatedCurrentReleaseRate = currentReleaseRate + flowRate
                let maxOpeningValve =
                    match Map.tryFind exit complexSites with
                    | Some site ->
                        maxReliefForBranch
                            (currentMinute + 2)
                            updatedCumulativeRelease
                            updatedOpenValves
                            updatedCurrentReleaseRate
                            beenHereBefore site
                    | None ->
                        maxReliefForLine
                            (currentMinute + 2)
                            updatedCumulativeRelease
                            updatedOpenValves
                            updatedCurrentReleaseRate
                            beenHereBefore
                            exit
                max maxWithoutOpeningValve maxOpeningValve
            else maxWithoutOpeningValve
    maxReliefForBranch 1 0 Set.empty 0 Set.empty startSite

let findMaximumRelief totalTime network =
    let (SummarizedNetwork (chains, chainsByStart, complexSites, allSites)) = network
    let startSite = complexSites["AA"]
    let rec maxReliefForBranch
            (currentMinute:int)
            (culumativeRelease:int)
            openValves
            currentReleaseRate
            (beenHereBefore:Set<string*int>) // To v with cumulative release r
            site =
        if currentMinute > totalTime then culumativeRelease
        else
            let (ValveSite (valve, flowRate, exits)) = site
            let (Valve fromValveName) = valve
            let couldOpen = (flowRate > 0) && (not (Set.contains valve openValves))
            exits
            |> Seq.filter (fun (Valve toValve) ->
                not (beenHereBefore |> Set.contains (toValve, currentReleaseRate)))
            |> Seq.map (fun exit ->
                let (Valve toValve) = exit
                maxForValve
                    currentMinute
                    culumativeRelease
                    openValves
                    currentReleaseRate
                    (beenHereBefore |> Set.add (toValve, currentReleaseRate))
                    valve
                    flowRate
                    exit
                    couldOpen)
            |> Seq.append [0] // Sequence will be empty if we already tried all options before
            |> Seq.max
    and maxForValve
        currentMinute
        culumativeRelease
        openValves
        currentReleaseRate
        beenHereBefore
        valve
        flowRate
        (Valve exit)
        couldOpen =
        if currentMinute > totalTime then culumativeRelease
        else
            let (Valve fromValveName) = valve
            //printf "%s%s %d\n" (System.String (' ', currentMinute - 1)) fromValveName culumativeRelease
            let maxWithoutOpeningValve =
                    maxReliefForBranch
                        (currentMinute + 1)
                        culumativeRelease
                        openValves
                        currentReleaseRate
                        beenHereBefore
                        allSites[exit]
            if couldOpen then
                let updatedOpenValves = Set.add valve openValves
                let minutesForWhichValveWillRemainOpen = totalTime - (currentMinute)
                let totalReliefFromThisValve = flowRate * minutesForWhichValveWillRemainOpen
                let updatedCumulativeRelease = culumativeRelease + totalReliefFromThisValve
                let updatedCurrentReleaseRate = currentReleaseRate + flowRate
                //printf "%s%s (O) %d\n" (System.String (' ', currentMinute)) fromValveName updatedCumulativeRelease
                let maxOpeningValve =
                        maxReliefForBranch
                            (currentMinute + 2)
                            updatedCumulativeRelease
                            updatedOpenValves
                            updatedCurrentReleaseRate
                            beenHereBefore
                            allSites[exit]
                max maxWithoutOpeningValve maxOpeningValve
            else maxWithoutOpeningValve
    maxReliefForBranch 1 0 Set.empty 0 (Set.empty |> Set.add ("AA", 0)) startSite

// A completely different way to approach this might be to try to summarize sections.
// If are at HH at time T, it could be turned on at a rate of 22 for the remaining
// 30-T minutes. (E.g., if you were at HH at the start of minute 1, you could have the
// valve open at the end of minute 1, providing 29 minutes of flow.)
// So we could represent JJ as (fun arrivedAtMinute -> 22 * (maxMinutes - arrivedAtMinute))

// What would a breadth-first approach look like? We'd want to start culling as soon as
// possible. So let's look at that:
// 0: AA: 0
// 1: (AA,BB):0, (AA,II):0, (AA,DD):0
// Not yet obvious which is the most promising.
//
// 2: AA,BB: 0 ->
//      (AA,BB,O)-(BB):13*28 = 364
//      (AA,BB,CC):0
//    AA,DD: 0 ->
//      (AA,DD,O)-(DD):20*28 = 560
//      (AA,DD,CC): 0
//      (AA,DD,EE): 0
//    AA,II: 0 ->
//      (AA,II,JJ): 0
// Although AA,DD,O is clearly in the lead, can we learn anything from that?
//
// 3:
//    (AA,BB,O)-(BB):364 ->
//      (AA,BB,O,AA)-(BB):364
//      (AA,BB,O,CC)-(BB):364
//    (AA,BB,CC):0 ->
//      (AA,BB,CC,O)-(CC):2*27 = 54
// X1   (AA,BB,CC,DD):0
//    (AA,DD,O)-(DD):560 ->
//      (AA,DD,O,CC)-(DD):560
//      (AA,DD,O,AA)-(DD):560
//      (AA,DD,O,EE)-(DD):560
//    (AA,DD,CC): 0 ->
//      (AA,DD,CC,O)-(CC):2*27 = 54
// X2   (AA,DD,CC,BB): 0
//    (AA,DD,EE): 0 ->
//      (AA,DD,EE,O)-(EE):3*27 = 81
//      (AA,DD,EE,FF): 0
//    (AA,II,JJ): 0 ->
//      (AA,II,JJ,O)-(JJ):21*27 = 567
//  Discards:
//  X1  At DD with nothing open. Shorter (AA,DD) gets to same state.
//  X2  At BB with nothing open. Shorter (AA,BB) gets to same state.
//
// 4:
//    (AA,BB,O,AA)-(BB):364 ->
//      (AA,BB,O,AA,DD)-(BB):364
//      (AA,BB,O,AA,II)-(BB):364
//    (AA,BB,O,CC)-(BB):364 ->
//      (AA,BB,O,CC,O)-(BB,CC):364 + 2*26 = 364 + 52 = 416
// X3   (AA,BB,O,CC,DD)-(BB):364
//    (AA,BB,CC,O)-(CC):54
//      (AA,BB,CC,O,DD)-(CC):54
//      (AA,BB,CC,O,BB)-(CC):54
//    (AA,II,JJ,O)-(JJ):567 ->
//      (AA,II,JJ,O,II)-(JJ):567
//    (AA,DD,O,CC)-(DD):560
//      (AA,DD,O,CC,O)-(CC,DD):560 + 2*26 = 560 + 52 = 612
//      (AA,DD,O,CC,BB)-(DD):560
//    (AA,DD,O,AA)-(DD):560
//      (AA,DD,O,AA,II)-(DD):560
//    (AA,DD,O,EE)-(DD):560
//      (AA,DD,O,EE,O)-(DD,EE):560 + 3*26 = 560 + 78 = 638
//      (AA,DD,O,EE,FF)-(DD):560
//    (AA,DD,CC,O)-(CC):2*27 = 54
// X1   (AA,DD,CC,O,BB)-(CC):54
// X2   (AA,DD,CC,O,DD)-(CC):54
//    (AA,DD,EE,O)-(EE):81
//      (AA,DD,EE,O,DD)-(EE):81
//      (AA,DD,EE,O,FF)-(EE):81
//    (AA,DD,EE,FF): 0
//      (AA,DD,EE,FF,GG): 0
// Discard reasons:
//  X1  This puts as at DD with valve CC open. There's another route (AA,BB,CC,O,BB) that
//      also leaves us at BB with valve CC open after 5 steps with the same score. We could
//      discard either because they are equivalent.
//  X2  At DD with CC open. (AA,BB,CC,O,DD) has same effect.
//  X3  At DD with BB open, score 364. (AA,BB,O,AA,DD) achieves same effect and score.
//
// 5:
//    (AA,BB,O,AA,DD)-(BB):364 ->
//      (AA,BB,O,AA,DD,O)-(BB,DD):364 + 20*25 = 364 + 500 = 864
// X1   (AA,BB,O,AA,DD,CC)-(BB):364
//    (AA,BB,O,AA,II)-(BB):364 ->
//      (AA,BB,O,AA,II,JJ)-(BB):364
//    (AA,BB,O,CC,O)-(BB,CC):416 ->
//      (AA,BB,O,CC,O,BB)-(BB,CC):416
//      (AA,BB,O,CC,O,DD)-(BB,CC):416
//    (AA,BB,CC,O,DD)-(CC):54 ->
// X2   (AA,BB,CC,O,DD,O)-(CC,DD):54 + 20*25 = 54 + 500 = 554
//      (AA,BB,CC,O,DD,AA)-(CC):54
//    (AA,BB,CC,O,BB)-(CC):54 ->
// X3   (AA,BB,CC,O,BB,O)-(BB,CC):54 + 13*25 = 54 + 325 = 379
// X4   (AA,BB,CC,O,BB,AA)-(CC):54
//    (AA,DD,O,CC,O)-(CC,DD):612 ->
//      (AA,DD,O,CC,O,BB)-(CC,DD):612
//      (AA,DD,O,CC,O,DD)-(CC,DD):612
//    (AA,DD,O,CC,BB)-(DD):560 ->
//      (AA,DD,O,CC,BB,O)-(BB,DD):560 + 13*25 = 560 + 325 = 885
// X5   (AA,DD,O,CC,BB,AA)-(DD):560
//    (AA,DD,O,AA,II)-(DD):560 ->
//      (AA,DD,O,AA,II,JJ)-(DD):560
//    (AA,DD,O,EE,O)-(DD,EE):638 ->
//      (AA,DD,O,EE,O,DD)-(DD,EE):638
//      (AA,DD,O,EE,O,FF)-(DD,EE):638
//    (AA,DD,O,EE,FF)-(DD):560 ->
//      (AA,DD,O,EE,FF,GG)-(DD):560
//    (AA,DD,EE,O,DD)-(EE):81 ->
// X6   (AA,DD,EE,O,DD,O)-(DD,EE):81 + 20*25 = 81 + 500 = 581
//      (AA,DD,EE,O,DD,AA)-(EE):81
//      (AA,DD,EE,O,DD,CC)-(EE):81
//    (AA,DD,EE,O,FF)-(EE):81 ->
//      (AA,DD,EE,O,FF,GG)-(EE):81
//    (AA,DD,EE,FF,GG): 0 ->
//      (AA,DD,EE,FF,GG,HH): 0
//    (AA,II,JJ,O,II)-(JJ):567 ->
//      (AA,II,JJ,O,II,AA)-(JJ):567
//
// Discards:
//  X1  At CC with BB open, 364. Shorter (AA,BB,O,CC,O) path gets to same point with BB (and
//      also CC) open, and higher score of 416.
//  X2  At DD with CC,DD open, 554. (AA,DD,O,CC,O,DD) path gets same state with 612.
//  X3  At BB with BB,CC open, 379. (AA,BB,O,CC,O,BB) gets same state with 416.
//  X4  At AA with CC open, 54, equivalent to (AA,BB,CC,O,DD,AA)
//  X5  At AA with DD open, 560. Shorter (AA,DD,O,AA) gets same state and score 560.
//  X6  At DD with DD,EE open, 581. (AA,DD,O,EE,O,DD) get same state with 638.
//
// 6:
//    (AA,BB,O,AA,DD,O)-(BB,DD):864 ->
// X1   (AA,BB,O,AA,DD,O,AA)-(BB,DD):864
// X2   (AA,BB,O,AA,DD,O,CC)-(BB,DD):864
//      (AA,BB,O,AA,DD,O,EE)-(BB,DD):864
//    (AA,BB,O,AA,II,JJ)-(BB):364 ->
//      (AA,BB,O,AA,II,JJ,O)-(BB):364 + 21*24 = 364 + 504 = 868
//    (AA,BB,O,CC,O,BB)-(BB,CC):416 ->
// X3   (AA,BB,O,CC,O,BB,AA)-(BB,CC):416
//    (AA,BB,O,CC,O,DD)-(BB,CC):416 ->
//      (AA,BB,O,CC,O,DD,O)-(BB,CC,DD):416 + 20*24 = 416 + 480 = 896
//      (AA,BB,O,CC,O,DD,AA)-(BB,CC):416
//      (AA,BB,O,CC,O,DD,EE)-(BB,CC):416
//    (AA,BB,CC,O,DD,AA)-(CC):54 ->
// X4   (AA,BB,CC,O,DD,AA,BB)-(CC):54
//      (AA,BB,CC,O,DD,AA,II)-(CC):54
//    (AA,II,JJ,O,II,AA)-(JJ):567 ->
//      (AA,II,JJ,O,II,AA,BB)-(JJ):567
//      (AA,II,JJ,O,II,AA,DD)-(JJ):567
//    (AA,DD,O,CC,O,BB)-(CC,DD):612 ->
//      (AA,DD,O,CC,O,BB,O)-(BB,CC,DD):612 + 13*24 = 612 + 312 = 924
//      (AA,DD,O,CC,O,BB,AA)-(CC,DD):612
//    (AA,DD,O,CC,O,DD)-(CC,DD):612 ->
// X5   (AA,DD,O,CC,O,DD,AA)-(CC,DD):612
//      (AA,DD,O,CC,O,DD,EE)-(CC,DD):612
//    (AA,DD,O,CC,BB,O)-(BB,DD):885 ->
//      (AA,DD,O,CC,BB,O,AA)-(BB,DD):885
//      (AA,DD,O,CC,BB,O,CC)-(BB,DD):885
//    (AA,DD,O,AA,II,JJ)-(DD):560 ->
//      (AA,DD,O,AA,II,JJ,O)-(DD,JJ):560 + 21*24 = 560 + 504 = 1064
//    (AA,DD,O,EE,O,DD)-(DD,EE):638 ->
//      (AA,DD,O,EE,O,DD,AA)-(DD,EE):638
//      (AA,DD,O,EE,O,DD,CC)-(DD,EE):638
//    (AA,DD,O,EE,O,FF)-(DD,EE):638 ->
//      (AA,DD,O,EE,O,FF,GG)-(DD,EE):638
//    (AA,DD,O,EE,FF,GG)-(DD):560 ->
//      (AA,DD,O,EE,FF,GG,HH)-(DD):560
//    (AA,DD,EE,O,DD,AA)-(EE):81->
//      (AA,DD,EE,O,DD,AA,BB)-(EE):81
//      (AA,DD,EE,O,DD,AA,II)-(EE):81
//    (AA,DD,EE,O,DD,CC)-(EE):81 ->
//      (AA,DD,EE,O,DD,CC,O)-(CC,EE):81 + 13*24 = 81 + 312 = 393
// X6   (AA,DD,EE,O,DD,CC,BB)-(EE):81
//    (AA,DD,EE,O,FF,GG)-(EE):81 ->
//      (AA,DD,EE,O,FF,GG,HH)-(EE):81
//    (AA,DD,EE,FF,GG,HH): 0 ->
//      (AA,DD,EE,FF,GG,HH,O)-(HH):0 + 22*24 = 528
//
// Discards:
//  X1  At AA with BB,DD open, 864. (AA,DD,O,CC,BB,O,AA) gets same state with 885.
//  X2  At CC with BB,DD open, 864. (AA,DD,O,CC,BB,O,CC) gets same state with 885.
//  X3  At AA with BB,CC open, 416. (AA,BB,O,CC,O,DD,AA) gets same state and score.
//  X4  At BB with CC open, 54. (AA,BB,O,CC,O,BB) gets to same point with BB (and also CC)
//          open, and a score of 416.
//  X5  At AA with CC,DD open, 612. (AA,DD,O,CC,O,BB,AA) gets same state and score.
//  X6  At BB with EE open, 81. (AA,DD,EE,O,DD,AA,BB) gets same state and score.
//
// 7:
//    (AA,BB,O,AA,DD,O,EE)-(BB,DD):864 ->
//      (AA,BB,O,AA,DD,O,EE,FF)-(BB,DD):864
//    (AA,BB,O,AA,II,JJ,O)-(BB):868 ->
//      (AA,BB,O,AA,II,JJ,O,II)-(BB):868
//    (AA,BB,O,CC,O,DD,O)-(BB,CC,DD):896 ->
//      (AA,BB,O,CC,O,DD,O,AA)-(BB,CC,DD):896
//      (AA,BB,O,CC,O,DD,O,CC)-(BB,CC,DD):896
//      (AA,BB,O,CC,O,DD,O,EE)-(BB,CC,DD):896
//    (AA,BB,O,CC,O,DD,AA)-(BB,CC):416 ->
//      (AA,BB,O,CC,O,DD,AA,BB)-(BB,CC):416
//      (AA,BB,O,CC,O,DD,AA,II)-(BB,CC):416
//    (AA,BB,O,CC,O,DD,EE)-(BB,CC):416->
//      (AA,BB,O,CC,O,DD,EE,O)-(BB,CC):416 + 3*23 = 416 + 69 = 485
//      (AA,BB,O,CC,O,DD,EE,FF)-(BB,CC):416
//    (AA,BB,CC,O,DD,AA,II)-(CC):54 ->
//      (AA,BB,CC,O,DD,AA,II,JJ)-(CC):54
//    (AA,II,JJ,O,II,AA,BB)-(JJ):567
//    (AA,II,JJ,O,II,AA,DD)-(JJ):567
//    (AA,DD,O,CC,O,BB,O)-(BB,CC,DD):612 + 13*24 = 612 + 312 = 924
//    (AA,DD,O,CC,O,BB,AA)-(CC,DD):612
//    (AA,DD,O,CC,O,DD,EE)-(CC,DD):612
//    (AA,DD,O,CC,BB,O,AA)-(BB,DD):885
//    (AA,DD,O,CC,BB,O,CC)-(BB,DD):885
//    (AA,DD,O,AA,II,JJ,O)-(DD,JJ):560 + 21*24 = 560 + 504 = 1064
//    (AA,DD,O,EE,O,DD,AA)-(DD,EE):638
//    (AA,DD,O,EE,O,DD,CC)-(DD,EE):638
//    (AA,DD,O,EE,O,FF,GG)-(DD,EE):638
//    (AA,DD,O,EE,FF,GG,HH)-(DD):560
//    (AA,DD,EE,O,DD,AA,BB)-(EE):81
//    (AA,DD,EE,O,DD,AA,II)-(EE):81
//    (AA,DD,EE,O,DD,CC,O)-(CC,EE):81 + 13*24 = 81 + 312 = 393
//    (AA,DD,EE,O,FF,GG,HH)-(EE):81
//    (AA,DD,EE,FF,GG,HH,O)-(HH):0 + 22*24 = 528
//

// We need to keep track of: the maximum score seen for every distinct (location, open valves)
// combination. This includes scores of 0, because we want to cull longer paths that get
// to the same location with a score of 0. E.g. (AA,BB,CC,DD,EE):0 should be culled because
// we already saw (AA,DD,EE):0.
// During cull, we need to be careful when weeding out ties - don't remove both!

type PathStep = PathMoveStep of valveName:string | PathOpenStep

type LocationInBreadthSearch =
    LocationInBreadthSearch of reversedPath:(PathStep list) * openValves:Set<string> * score:int
let currentPositionNameFromReversedPath path =
    match path with
    | PathMoveStep move::_ -> move
    | PathOpenStep::PathMoveStep move::_ -> move
    | _ -> failwith "Path should end either with [PathMoveStep] or [PathMoveStep; PathOpenStep]"


let initialLocation = LocationInBreadthSearch ([PathMoveStep "AA"], Set.empty, 0)
let candidateNextLocations network currentLocation =
    let (SummarizedNetwork (_, _, _, sites)) = network
    let (LocationInBreadthSearch (currentReversedPath, openValves, score)) = currentLocation
    let currentPositionName = currentPositionNameFromReversedPath currentReversedPath
    match sites |> Map.tryFind currentPositionName with
    | Some (ValveSite (_, flowRate, tunnels)) ->
        let openStepIfApplicable =
            if (flowRate > 0) && (not (openValves |> Set.contains currentPositionName)) then
                let minutesRemaining = 30 - (currentReversedPath.Length)
                LocationInBreadthSearch (
                        PathOpenStep::currentReversedPath,
                        openValves |> Set.add currentPositionName,
                        score + flowRate * minutesRemaining)
                |> Seq.singleton
            else Seq.empty
        let moveSteps =
            tunnels
            |> Seq.sort
            |> Seq.map (fun (Valve nextLocation) ->
                LocationInBreadthSearch (
                    PathMoveStep nextLocation::currentReversedPath,
                        openValves,
                        score))
        Seq.concat [openStepIfApplicable; moveSteps]
    | _ -> failwithf "Failed to find site %s" currentPositionName

candidateNextLocations testSummarized initialLocation |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0);
        LocationInBreadthSearch ([PathMoveStep "DD"; PathMoveStep "AA"], Set.empty, 0);
        LocationInBreadthSearch ([PathMoveStep "II"; PathMoveStep "AA"], Set.empty, 0);
    ]
candidateNextLocations
    testSummarized
    (LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0))
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (Set.ofList ["BB"]), 13 * 28);
        LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0);
    ]

candidateNextLocations
    testSummarized
    (LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (Set.ofList ["BB"]), 13 * 28))
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathMoveStep "AA"; PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (Set.ofList ["BB"]), 13 * 28);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (Set.ofList ["BB"]), 13 * 28);
    ]

type BreadthSearchState =
    BreadthSearchState of currentLocations:(LocationInBreadthSearch list) *
    maximaByLocationThenValves:Map<string, (Set<string> * int) list>
let initialBreadthSearchState = BreadthSearchState (
    [initialLocation], Map.empty |> Map.add "AA" [(Set.empty, 0)])

type CandidateComparisonResult = LeftIsBetter | RightIsBetter | LeftAndRightIdentical
type PartialOrderComparisonResult = PartialOrdered of result:CandidateComparisonResult | NeitherIsEvidentlyBetter
let isBetterCandidate leftPathLength leftOpenValves leftScore rightPathLength rightOpenValves rightScore =
    let valves =
        if leftOpenValves = rightOpenValves then PartialOrdered LeftAndRightIdentical
        // Having more valves open isn't actually necessarily better. If two paths lead to the
        // same position and the same score, but one has fewer valves open, it has more scope
        // to improve its score by opening those other valves.
        //else if (Set.isSubset leftOpenValves rightOpenValves) then PartialOrdered RightIsBetter
        //else if (Set.isSubset rightOpenValves leftOpenValves) then PartialOrdered LeftIsBetter
        else NeitherIsEvidentlyBetter

    let compareScores left right =
        if left = right then LeftAndRightIdentical
        else if left < right then RightIsBetter
        else LeftIsBetter
    let lengths = compareScores rightPathLength leftPathLength // Order inverted because shorter is better
    let scores = compareScores leftScore rightScore
    let combineOrders left right =
        match (left, right) with
        | (NeitherIsEvidentlyBetter _, _)
        | (_, NeitherIsEvidentlyBetter _) -> NeitherIsEvidentlyBetter
        | (PartialOrdered oleft, PartialOrdered oright) ->
            match (oleft, oright) with
            | (LeftAndRightIdentical, LeftAndRightIdentical) -> PartialOrdered LeftAndRightIdentical
            | (LeftIsBetter, RightIsBetter) -> NeitherIsEvidentlyBetter
            | (RightIsBetter, LeftIsBetter) -> NeitherIsEvidentlyBetter
            | (LeftIsBetter, _)
            | (_, LeftIsBetter) -> PartialOrdered LeftIsBetter
            | (RightIsBetter, _)
            | (_, RightIsBetter) -> PartialOrdered RightIsBetter

    combineOrders valves (combineOrders (PartialOrdered lengths) (PartialOrdered scores))
        
    //if (Set.isSubset leftOpenValves rightOpenValves) then
    //    // Right candidate has all valves open that left does.
    //    if leftPathLength > rightPathLength then
    //        // Right candidate is a shorter path to getting all the same valves open
    //        if leftScore >= rightScore then
    //            // Right candidate is a shorter path to getting all the same valves open
    //            // and has equal or higher score, so it's definitely better.
    //            RightIsBetter
    //        else
    //            // Right candidate is a shorter path to getting all the same valves open, but
    //            // has lower score, so it's not clear which is best
    //            NeitherIsEvidentlyBetter
    //    else if leftPathLength = rightPathLength then
    //        // Right has all same valves open as left poss
    //    if leftScore < rightScore then
    //        // Higher score and all the same valves (and perhaps more) open, so
    //        // this is definitely the better candidate
    //        RightIsBetter
    //    else if leftScore = rightScore then
    //        // Equal score
    //    else
    //        // Left s
    //else
    //if existingPositionScore >= candidateScore then
    //    //
    //else false

// Shorter path, same effect (no valves yet)
// (AA,BB,CC,DD):0 vs (AA,DD):0
isBetterCandidate 4 Set.empty 0 2 Set.empty 0 =! PartialOrdered RightIsBetter
isBetterCandidate 2 Set.empty 0 4 Set.empty 0 =! PartialOrdered LeftIsBetter

// Shorter path, same effect


// Same path length, same valves, same score
// (AA,BB,O,CC,DD)-(BB):364 vs (AA,BB,O,AA,DD)-(BB):364
isBetterCandidate 5 (Set.singleton "BB") 364 5 (Set.singleton "BB") 364 =! PartialOrdered LeftAndRightIdentical

// Same path length, same valves, different score
// (AA,BB,CC,O,DD,O)-(CC,DD):554 vs (AA,DD,O,CC,O,DD):612
isBetterCandidate 6 (Set.ofList ["CC";"DD"]) 554 6 (Set.ofList ["CC";"DD"]) 612 =! PartialOrdered RightIsBetter
isBetterCandidate 6 (Set.ofList ["CC";"DD"]) 612 6 (Set.ofList ["CC";"DD"]) 554 =! PartialOrdered LeftIsBetter

// Same path length, different non-subset valves (different score, but doesn't matter)
// (AA,BB,O,AA,DD,O)-(BB,DD):864 vs (AA,BB,O,CC,O,DD)-(BB,CC):416
isBetterCandidate 6 (Set.ofList ["BB";"DD"]) 864 6 (Set.ofList ["BB";"CC"]) 416 =! NeitherIsEvidentlyBetter
isBetterCandidate 6 (Set.ofList ["BB";"CC"]) 416 6 (Set.ofList ["BB";"DD"]) 864 =! NeitherIsEvidentlyBetter

// Test valve subsets?

let filterCandidateLocationBasedOnState
    (BreadthSearchState (_, maximaByLocationAndValves))
    (LocationInBreadthSearch (candidateReversedPath, candidateOpenValves, candidateScore)) =
    let candidateLocationName = currentPositionNameFromReversedPath candidateReversedPath
    match maximaByLocationAndValves |> Map.tryFind candidateLocationName with
    | Some maximaByValves ->
        let atLeastOneEquivalentOrBetterExists =
            maximaByValves
            |> List.exists (fun (existingPositionOpenValves, existingPositionScore) ->
                (Set.isSubset candidateOpenValves existingPositionOpenValves) &&
                    existingPositionScore >= candidateScore)
        not atLeastOneEquivalentOrBetterExists
    | None -> true

candidateNextLocations
    testSummarized
    (LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0))
    |> Seq.filter (filterCandidateLocationBasedOnState initialBreadthSearchState)
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (Set.ofList ["BB"]), 13*28);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathMoveStep "BB"; PathMoveStep "AA"], Set.empty, 0);
    ]

// Having produced a list of candidates and removed any that are demonstrably less good
// than states we were already in earlier, two problems remain:
//  1: this round of candidates might contain entries that are demonstrably less good than
//      other candidates produced in the same round that get us to the same position
//      and open valve sets
//  2: this round of candidates might contain entries that are effectively the same (e.g.,
//      two different routes through the system that end up in the same place, with the
//      same valves open, and equal scores)
// We want to remove any that match 1. And for 2, we want to just pick one (arbitrarily).
// It might seems like we could solve both by grouping by (position, valves open), and picking
// any entry with the maximum score with that group. However, 1 is more subtle, because
// "demonstrably less good" also includes cases where the open valve set of one candidate
// is a subset of the open valve set of another. E.g. being at AA with score 100 and BB open
// is demonstrably less good thab being at AA with score 100 and (BB,CC) open. We will miss
// that if we group by (position, valves open). (It's not clear whether we would actually
// miss any because in most scenarios, having additional valves open would come with
// a higher total score. But it would be possible to contrive scenarios where that's
// not true.)

let reduceCandidatesToSingleBest candidates =
    let candidatesByCurrentLocation =
        candidates
        |> Seq.fold
            (fun bestCandidatesByLocation location ->
                let (LocationInBreadthSearch (reversedPath, openValves, score)) = location
                let locationName = currentPositionNameFromReversedPath reversedPath
                Map.add
                    locationName
                    (match bestCandidatesByLocation |> Map.tryFind locationName with
                    | Some candidates -> location::candidates
                    | None -> [location])
                    bestCandidatesByLocation)
            (Map.empty)
    candidatesByCurrentLocation
    |> Map.toSeq
    |> Seq.collect
        (fun (currentLocation, allCandidatesLeadingHere) ->
            // We want to pick the best. In cases where there is a tie,
            // we pick one. In cases where there are multiple potential 'best'
            // values (because this is a partial order) we want to pick all
            // of them.
            // The manual process was:
            //  For each item, inspect all the others and see if any got a higher or equal
            //  score. If one got a higher score, or if the equal-scoring one appeared before
            //  this one, discard this one. Otherwise, retain this one.

            allCandidatesLeadingHere
            |> Seq.mapi (fun i v -> (i, v))
            |> Seq.scan
                (fun (_, indicesAlreadyExcluded) (firstIndex, firstCandidate) ->
                    if indicesAlreadyExcluded |> Set.contains firstIndex then
                        (None, indicesAlreadyExcluded)
                    else
                        let (betterEntryExists, updatedIndicesToExclude) =
                            allCandidatesLeadingHere
                            |> Seq.mapi (fun i v -> (i, v))
                            |> Seq.skip firstIndex
                            |> Seq.fold
                                (fun (alreadyFoundBetterEntry:bool, indicesToExclude:Set<int>) (secondIndex:int, secondCandidate:LocationInBreadthSearch) ->
                                    if alreadyFoundBetterEntry then
                                        (true, indicesToExclude)
                                    else if indicesToExclude |> Set.contains secondIndex then
                                        (false, indicesToExclude)
                                    else if firstIndex = secondIndex then
                                        (false, indicesToExclude)
                                    else
                                    let (LocationInBreadthSearch (firstPath, firstOpenValves, firstScore)) = firstCandidate
                                    let (LocationInBreadthSearch (secondPath, secondOpenValves, secondScore)) = secondCandidate
                                    match isBetterCandidate firstPath.Length firstOpenValves firstScore secondPath.Length secondOpenValves secondScore with
                                    | PartialOrdered LeftIsBetter -> (false, Set.add secondIndex indicesToExclude)
                                    | PartialOrdered RightIsBetter -> (true, Set.add firstIndex indicesToExclude)

                                    // This pair is a tie, so we pick the first.
                                    | PartialOrdered LeftAndRightIdentical ->
                                        if firstIndex < secondIndex then
                                            (false, Set.add secondIndex indicesToExclude)
                                        else
                                            (true, Set.add firstIndex indicesToExclude)
                                    // Pair is not comparable, so let both through
                                    | _ -> (false, indicesToExclude))
                                (false, indicesAlreadyExcluded)
                        (
                            (if betterEntryExists then None else (Some firstCandidate)),
                            updatedIndicesToExclude
                        ))
                (None, Set.empty)
            |> Seq.choose fst)


reduceCandidatesToSingleBest
    [LocationInBreadthSearch ([PathMoveStep "AA"], Set.empty, 0)] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"], Set.empty, 0)]

reduceCandidatesToSingleBest
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], Set.ofList ["CC"], 54);
     LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], Set.ofList ["CC"], 54)] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], Set.ofList ["CC"], 54)]

reduceCandidatesToSingleBest
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"; PathOpenStep], Set.ofList ["CC";"DD"], 554);
     LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathOpenStep; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"], Set.ofList ["CC";"DD"], 612)
     ] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathOpenStep; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"], Set.ofList ["CC";"DD"], 612)]


let walkNetwork network =
    Seq.unfold
        (fun state ->
            let (BreadthSearchState (currentLocations, currentMaxima)) = state
            let candidates =
                currentLocations
                |> Seq.collect (candidateNextLocations network)
            let candidatesNotObviouslyWorseThanEarlierLocations =
                candidates
                |> Seq.filter (filterCandidateLocationBasedOnState state)
            let bestNextLocations =
                reduceCandidatesToSingleBest candidatesNotObviouslyWorseThanEarlierLocations
                |> List.ofSeq
            let newMaxima =
                bestNextLocations
                |> Seq.fold
                    (fun maxima location ->
                        let (LocationInBreadthSearch (currentReversedPath, openValves, score)) = location 
                        let currentPositionName = currentPositionNameFromReversedPath currentReversedPath
                        match Map.tryFind currentPositionName maxima with
                        | Some positionMaxima ->
                            // Here, positionMaxima is a list of (open valve, score) entries for
                            // previously discovered routes to currentPositionName. We need to
                            // see whether this new LocationInBreadthSearch either adds a new open
                            // valve set for this position, or achieves a higher score for an open
                            // valve set already seen.
                            let (stillToAdd, updatedMaxima) =
                                positionMaxima
                                |> List.fold
                                    (fun (finished, updatedList) maximum ->
                                        // If we already either replaced an existing maximum with our
                                        // new location, or determined that an existing maximum achieved
                                        // the same valve set with a better score than our new location,
                                        // then we're basically done, and just need to pass all remaining
                                        // maxima through.
                                        if finished then (true, maximum::updatedList)
                                        else
                                            let (thisOpenValves, thisScore) = maximum
                                            if thisOpenValves = openValves then
                                                if score > thisScore then
                                                    // The position we're looking to add has the
                                                    // same open valves and a higher score than an
                                                    // existing position, so we record it as the
                                                    // new maximum.
                                                    (true, (openValves, score)::updatedList)
                                                else
                                                    // This position has the same valves but a lower
                                                    // than or equal score, so we retain the existing
                                                    // maximum.
                                                    (true, maximum::updatedList)
                                            else
                                                // Not a match, so pass through the existing maximum
                                                // and keep looking.
                                            (false, maximum::updatedList))
                                    (false, [])
                            maxima
                        | None -> Map.add currentPositionName [(openValves, score)] maxima
                        )
                    currentMaxima
            Some (bestNextLocations, BreadthSearchState (bestNextLocations, newMaxima)))
        initialBreadthSearchState

let displayPathStep step =
    match step with
    | PathOpenStep -> "O"
    | PathMoveStep l -> l

let displayLocation (LocationInBreadthSearch (reversedPath, openValves, score)) =
    sprintf "%d: %s-(%A)" score (System.String.Join(", ", (Seq.rev reversedPath |> Seq.map displayPathStep))) openValves

let solvePart1 summarized =
    for locations in ((walkNetwork summarized) |> Seq.take 30) do
        //let sortedLocations =
        //    locations
        //    |> Seq.sortBy
        //        (fun (LocationInBreadthSearch (reversedPath, _,_)) ->
        //            Seq.rev reversedPath
        //            |> Seq.map (fun p ->
        //                match p with
        //                | PathOpenStep -> " "
        //                | PathMoveStep l -> l)
        //            |> List.ofSeq)
        //for location in sortedLocations do
        //    printf "%s\n" (displayLocation location)
        printf "Max: %d\n" (locations |> Seq.map (fun (LocationInBreadthSearch (_,_,score)) -> score) |> Seq.max)
        printf "  %A\n\n" (locations |> Seq.maxBy (fun (LocationInBreadthSearch (_,_,score)) -> score) |> displayLocation)

solvePart1 testSummarized
solvePart1 summarized

// Another possible approach would be to work out what the fastest route to every
// valve is, because that would tell us the maximum score available from that. Could
// we do this as a series of rounds?
// 1:
//    BB - (AA,BB,O):13*28 = 364
//    CC - (AA,BB,CC,O):2*27 = 54
// X     - (AA,DD,CC,O):2*27 = 54
//    DD - (AA,DD,O):20*28 = 560
//    HH - (AA,DD,EE,FF,GG,HH,O):22*24 = 88
//    JJ - (AA,II,JJ,O): 21*27 = 520
// We can discard one of the CC routes because they both produce exactly the same result.
// It doesn't matter which.
// 2:
//    BB,CC - (AA,BB,O,CC,O):364 + 26*2 = 364 + 52 = 416
//    BB,DD - (AA,BB,O,AA,DD,O):364 + 25*20 = 364 + 500 = 864
// X          (AA,BB,O,CC,DD,O):364 + 25*20
//    BB,HH - (AA,BB,O,AA,DD,EE,FF,GG,HH,O):364 + 21*22 = 364 + 462 = 826
// X          (AA,BB,O,CC,DD,EE,FF,GG,HH,O):364 + 21*22
//    BB,JJ - (AA,BB,O,AA,II,JJ,O):364 + 24*21 + 504 = 868
//    CC,BB - (AA,BB,CC,O,BB,O):54 + 25*13
//    CC,DD
//    CC,HH
//    CC,JJ
//    DD,BB
//    DD,CC
//    DD,HH
//    DD,JJ
//    HH,BB
//    HH,CC
//    HH,DD
//    HH,JJ
//    JJ,BB
//    JJ,CC
//    JJ,DD
//    JJ,HH




// Input has 15 nodes with a non-zero flow rate


//printf "Test max: %d\n" (findMaximumRelief 30 testSummarized)
//printf "Read max: %d\n" (findMaximumRelief 30 summarized)
