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
//  /-->  AA <-> II <-> JJ
//  |     ^
//  v     |
//  BB    |
//  ^     |
//  |     |
//  v     v
//  CC -> DD <-> EE <-> FF <-> GG <-> HH
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
//printf "%A\n" testWaysIn
let waysIn = getWaysInMap sites

let findTermini (waysIn:Map<string, string list>) =
    waysIn
    |> Map.toSeq
    |> Seq.filter (fun (_, waysIn) -> (List.length waysIn) = 1)

let testTermini = findTermini testWaysIn
//printf "%A\n" (testTermini |> List.ofSeq)
let termini = findTermini waysIn
printf "%A\n" (termini |> List.ofSeq)

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

printf "All lobes: %A\n" (findAllLobesForTermini termini waysIn |> List.ofSeq)

// Disappointingly, lobes account for only 15 of the sites. That doesn't pare the problem space down enough.

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
        * complexSites:ValveSite list

let summarizeNetwork (sites:ValveSite list) =
    sites
    |> Seq.fold
        (fun summarizedNetwork valveSite ->
            let addUnidirectionalChainElement fromValve toValve (SummarizedNetwork (chains, complexSites)) =
                let matchingKeys =
                    chains
                    |> Map.keys
                    |> Seq.filter (fun (k1, k2) ->
                        (toValve = k1) || (fromValve = k2)
                        //&& (not ((toValve = k1) && (fromValve = k2))))
                        )
                    |> List.ofSeq
                //match (Map.keys chains |> Seq.filter (fun (k1, k2) -> (toValve = k1)  (fromValve = k2)) |> List.ofSeq) with
                match matchingKeys with
                | [] ->
                    SummarizedNetwork (
                        chains |> Map.add (fromValve, toValve) (Set.ofList [fromValve; toValve]),
                        complexSites)
                | keys ->
                    keys
                    |> Seq.fold
                        (fun (SummarizedNetwork (chains, complexSites)) (k1, k2) ->
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
                            
                            //if (Set.contains valveSite chainMembers)
                            //    || (before && (fromValve = k2))
                            //    || ((not before) && (toValve = k1)) then
                            if (before && ((fromValve = k2)
                                           || chainMembers |> Set.contains fromValve))
                                || ((not before) && ((toValve = k1)
                                        || chainMembers |> Set.contains toValve)) then
                                // This is pointer back up the chain, so don't try to add this.
                                SummarizedNetwork (chains, complexSites)
                            else
                                let chainsExceptThis = Map.remove (k1, k2) chains
                                let updatedMap =
                                    if before then
                                        let updatedChain = chainMembers |> Set.add fromValve
                                        // This site extends the start of the chain
                                        // fromValve..k1/toValve..(chain)..k2
                                        chainsExceptThis |> Map.add (fromValve, k2) updatedChain
                                    else
                                        let updatedChain = chainMembers |> Set.add toValve
                                        // This site extends the end of the chain
                                        // k1..(chain)..k2/fromValve..toValve
                                        chainsExceptThis |> Map.add (k1, toValve) updatedChain
                                SummarizedNetwork (updatedMap, complexSites))
                        (SummarizedNetwork (chains, complexSites))

            match valveSite with
            | ValveSite (Valve valve, _, [Valve t1; Valve t2]) ->
                summarizedNetwork
                |> addUnidirectionalChainElement valve t1
                |> addUnidirectionalChainElement valve t2
            | ValveSite (Valve valve, _, [Valve t1]) ->
                addUnidirectionalChainElement valve t1 summarizedNetwork
            | _ ->
                let (SummarizedNetwork (chains, complexSites)) = summarizedNetwork
                SummarizedNetwork (chains, valveSite::complexSites))
        (SummarizedNetwork (Map.empty, []))

printf "Test summarized: %A\n" (summarizeNetwork testSites)

let summarized = (summarizeNetwork sites)
printf "Sites summarized: %A\n" summarized

// 10 complex sites and 50 connecting sequences.
// 
