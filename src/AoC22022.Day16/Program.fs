﻿open FParsec
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

// Here's how the test input looks:
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
// Or just the openings:
// DD20, BB13, JJ21, HH22, EE3, CC2

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

// Update: actually, it's not the travelling salesman problem, because the goal isn't
// just to visit all sites. My "better" solution does this:
//   27*JJ + 23*BB + 21*CC + 19*DD + 17*EE + 13*HH
// = 27*21 + 23*13 + 21*2  + 19*20 + 17*3  + 13*22
// = 567   + 299   + 42    + 380   + 51    + 286
// = 1625
// But the example sequence shown produces 1651. So although my path is shorter, theirs
// produces a higher score. How does it do that? Let's look at their sequence:
//  AA,DD,O,CC,BB,O,AA,II,JJ,O,II,AA,DD,EE,FF,GG,HH,O,GG,FF,EE,O,DD,CC,O
//   28*DD + 25*BB + 21*JJ + 13*HH + 10*EE + 7*C
// = 28*20 + 25*13 + 21*21 + 13*22 + 9*3  + 6*2
// = 560   + 325   + 441   + 286   + 27   + 12
// = 1651
//
// There are some surprising features of this sequence:
//  * it goes through CC twice but only opens it on the second visit!
//  * same for EE
//  * it goes all the way into a long cul de sac of DD,EE,FF,GG,HH, opens HH, then
//      retraces its steps all the way back up just so it can come back and open CC
//  * it prioritises DD (20) over JJ (21)
//
// These seem counterintuitive. If you're going to open a valve, why delay? The longer
// it's open, the more total pressure release you get. Why wouldn't you that trip
// out to the most distant node, HH, the last thing you did so you didn't have to
// spend all that time retracing your steps? Why wouldn't you open the highest value
// at the earliest opportunity.
//
// The first point is that the upside from getting to a high-value valve earlier can be
// greater than the downsides of not opening all valves as you're passing through. For
// example, My solution gets CC open early enough that it remains open 21 minutes,
// producing a total release of 42. The optimal path actually reaches CC earlier, and
// could have had it open for 26 minutes, which would have given a total of 52. But by
// skipping its first opportunity to open CC, the optimal path gets BB, JJ, HH, and EE
// open one step earlier than would be the case if CC had been opened earlier. That adds
// 13+21+22+3 = 59. Since bringing these four forward by just one step produces a gain of 59,
// and the total we could have got by opening CC earlier was just 52, the wins from
// skipping over CC outweigh the gain. (And the optimal path does eventually get CC open,
// for a total of 12, so the gain from opening earlier would have been just 40, compared
// against the gains of 59 from opening all those other valves one step sooner.)
//
// This then leaves the question of why the optimal solution doesn't appear to prioritise
// the highest value nodes in all cases. The highest flow rate node is JJ, at 21. My
// solution gets this open first, so it gets 27 minutes. The optimal solution has it
// open for just 21 minutes. Given that we've just seen how useful it can be to defer
// lower-valued nodes in order to get higher-values ones open sooner, why does it leave
// JJ for so long? This is more subtle. By sacrifing potential output from JJ, the
// optimal path is able to open DD and BB earlier. It gets DD open for 9 minutes longer
// than mine, giving 180 more points, which offsets the fact that it has JJ open for
// 6 minutes fewer, which turns out to lose it only 126 points. So that's clearly
// a worthwhile trade. So it's not as simple as getting the highest value valve open
// as early as possible. You want to get many high value valves open as early as you can,
// which is a fuzzier goal.


// One approach I intially considered was to simplify our map into lobes. Looking at
// the visualization of the example problem above, roughly speaking there's a 'central'
// cycle featuring AA, BB, CC, and DD, and two simple linear paths from this cluster
// heading out to JJ and HH. I'm calling those things "lobes". The thing that makes
// them interesting is the limited branching. There are basically two things you can do
// with a lobe:
//  1. go down it and come back up
//  2. go down it and never return
// (It's possible to imagine "terminal lobes" that permit only 2. The simple example
// has none, but it's possible our real input will.) In the simple example, my 18 step
// solution takes option 2 for the longer of the two lobes, which makes sense in a
// hand-wavey fashion: the longer the lobe, the higher the cost of returning from its
// extreme, so it seems plausible that the optimal route might entail leaving the longest
// lobe until last, so that you can avoid the cost of heading down it and back.
//
// What I should have spotted is that the optimal solution didn't actually do what my
// solution did: the optimal solution actually went all the way down the longest lobe
// and then all the way back again so that it could finally open CC, a valve it had
// already been through. So in practice, knowing about these 'lobes' isn't useful in
// in practice. Unfortunately, before I worked that out, I wrote code that inspected
// the map to work out where the lobes were.
//
// Disappointingly, lobes accounted for only 12 of the sites. More precisely:
// Test: 2 lobes of length 4 and 2 (8 sites)
//  EE FF GG HH
//  II JJ
// Real: 3 lobes of length 3, 6, and 3 (12 sites)
//  DC ST CR
//  EQ ZN HL DW LC JL
//  EX RB CC
// That doesn't pare the problem space down enough.
//
// My next thought was to ask what if, instead, we looked for lines (of which lobes are
// a special case)? Just look for non-branching node sequences? There might be a relatively
// small number of clusters joined mostly by lines.
//
// Again, had I looked more closely at the counterintuitive turns taken by the optimal
// path, I might have realised that this probably wasn't going to help much. It's not
// obvious why, when your map consists of a central cluster and two nodes (as the test input
// does) your optimal path would involve going out and back into that cluster twice,
// opening one of the valves in that cluster only on the final visit. But before realising
// that, I did a bit of analysis and determined that 47 of my inputs are non-branching.
// This is an important fact because it means that the search space won't explode at quite
// the rate a fully-interconnected graph would, but the fact that the need to balance
// multiple high-value nodes to achieve a non-obvious best result means you can't exploit
// this mostly-non-branching structure to go directly to an answer. You still need to
// explore, it's just that the exploration will be (just about) manageable.

// In the solution I eventually used, one operation happens a lot: comparing sets of open
// valves. For each candidate path through the network, I keep track of the valves that
// have been opened so far, and when trying to work out whether various candidate next
// moves are better than, worse than, or equivalent to other ones (to avoid exploring
// pointless options like going round in circles without ever opening a valve), it is
// often useful to ask "does this candidate have the same valves open as this other
// candidate?"
//
// Right now, I'm modeling open valve sets with a Set<string>, and profiling has revealed
// that we spend a lot of time inside the Set<T> comparison logic. Since there are never
// more than 64 valves, we could model this with a bitfield in a 64-bit integer, at which
// point set comparison becomes a simple value comparison. The tricky thing is mapping between
// node names and bit positions.
//
// What I'm thinking is we can observe that the names are always a pair of uppercase letters,
// giving us 26*26=676 possible names, so we could build a 676-byte lookup table mapping from
// name to bit position. And of course we can trivially build a lookup table from bit position
// to name.

type NodeNameSet = NodeNameSet of nameToBitIndex:(byte array) * bitIndexToName:(string array)
let nodeLabelToInt (name:string) =
    if name.Length <> 2 then failwithf "Node label must be of length 2, was '%s'" name
    let charToNum (c:char) =
        let result = (int c) - (int 'A')
        if result < 0 || result > 25 then failwithf "Node label letters must be in range A-Z, was '%c'" c
        result
    let lower = charToNum name[0]
    let higher = charToNum name[1]
    lower + (higher * 26)
let makeNodeNameSet (names:string seq) =
    let indexedNames =
        names
        |> Seq.sort // For cosmetic purposes only
        |> Seq.mapi (fun i v -> (byte i, v))
    let (nameToBitIndex, bitIndexToName) =
        Seq.foldBack
            (fun (i, label) ((nameToBitIndex:byte array), bitIndexToName) ->
                let labelNumber = nodeLabelToInt label
                if nameToBitIndex[labelNumber] <> 255uy then failwithf "Duplicate label: '%s'" label
                nameToBitIndex[labelNumber] <- i
                (nameToBitIndex, label::bitIndexToName))
            indexedNames
            ((Array.create (26*26) 255uy), List.empty)
    NodeNameSet (nameToBitIndex, Array.ofList bitIndexToName)

type NodeSet =
    struct
        val Members:uint64
        new(members:uint64) = { Members = members }
    end

let emptyNodeSet (names:NodeNameSet) = NodeSet 0UL
let nodeSetContains (nodeSetContained:NodeSet) (nodeSetOuter:NodeSet) =
    (nodeSetContained.Members &&& nodeSetOuter.Members) = nodeSetContained.Members
let combineNodeSets (nodeSet1:NodeSet) (nodeSet2:NodeSet) =
    NodeSet (nodeSet1.Members ||| nodeSet2.Members)

let nodeSetFromLabel (NodeNameSet (nameToBitIndex, _)) label =
    let labelNumber = nodeLabelToInt label
    let bitIndex = nameToBitIndex[labelNumber]
    NodeSet (1UL <<< int bitIndex)
let nodeSetFromLabels names labels =
    Seq.fold
        (fun nodeSet label -> combineNodeSets nodeSet (nodeSetFromLabel names label))
        (emptyNodeSet names)
        labels


let labelsFromNodeSet (NodeNameSet (_, bitIndexToName)) (nodeset:NodeSet) =
    Seq.unfold
        (fun (bits, i, labels) ->
            if bits = 0UL || i < 0 then None
            else
                let mask = 1UL <<< i
                let isSet = (bits &&& mask) <> 0UL
                let updatedLabels = 
                    if isSet then bitIndexToName[i]::labels
                    else labels
                Some (updatedLabels, (bits &&& ~~~mask, i - 1, updatedLabels)))
        (nodeset.Members, 63, List.empty)
    |> Seq.tryLast
    |> Option.defaultValue List.empty

type SummarizedNetwork =
    SummarizedNetwork
        of allSites:Map<string, ValveSite> *
        nodeNames:NodeNameSet

let summarizeNetwork (sites:ValveSite list) =
    let siteMap =
        sites
        |> Seq.fold 
            (fun sites valveSite ->
                let (ValveSite (Valve valve, _, _)) = valveSite
                sites |> Map.add valve valveSite)
            Map.empty
    SummarizedNetwork (siteMap, siteMap.Keys |> makeNodeNameSet)

let testSummarized = (summarizeNetwork testSites)
let (SummarizedNetwork (_, testNodeNames)) = testSummarized
let testEmptyNodeSet = emptyNodeSet testNodeNames
printf "Test summarized: %A\n" testSummarized

let summarized = (summarizeNetwork sites)
let (SummarizedNetwork (_, nodeNames)) = testSummarized
printf "Sites summarized: %A\n" summarized


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
    LocationInBreadthSearch of
        reversedPath:(PathStep list) *
        openValves:NodeSet *
        score:int *
        unopenedValveFlow:int
let currentPositionNameFromReversedPath path =
    match path with
    | PathMoveStep move::_ -> move
    | PathOpenStep::PathMoveStep move::_ -> move
    | _ -> failwith "Path should end either with [PathMoveStep] or [PathMoveStep; PathOpenStep]"


let getInitialLocation network =
    let (SummarizedNetwork (sites, nodeNames)) = network
    let totalFlow = sites.Values |> Seq.sumBy (fun (ValveSite (_, flowRate, _)) -> flowRate)
    LocationInBreadthSearch ([PathMoveStep "AA"], (emptyNodeSet nodeNames), 0, totalFlow)

let testInitialLocation = getInitialLocation testSummarized
let inputInitialLocation = getInitialLocation summarized

let candidateNextLocations network bestScoreFromLastRound currentLocation =
    let (SummarizedNetwork (sites, nodeNames)) = network
    let (LocationInBreadthSearch (currentReversedPath, openValves, score, unopenedValveFlow)) = currentLocation
    let minutesRemaining = 30 - (currentReversedPath.Length)
    let currentPositionName = currentPositionNameFromReversedPath currentReversedPath
    let currentPositionAsNodeSet = nodeSetFromLabel nodeNames currentPositionName
    match sites |> Map.tryFind currentPositionName with
    | Some (ValveSite (_, flowRate, tunnels)) ->
        let openStepIfApplicable =
            if (flowRate > 0) && (not (openValves |> nodeSetContains currentPositionAsNodeSet)) then
                LocationInBreadthSearch (
                        PathOpenStep::currentReversedPath,
                        openValves |> combineNodeSets currentPositionAsNodeSet,
                        score + flowRate * minutesRemaining,
                        unopenedValveFlow - flowRate)
                |> Seq.singleton
            else Seq.empty
        let moveSteps =
            tunnels
            |> Seq.sort
            |> Seq.map (fun (Valve nextLocation) ->
                LocationInBreadthSearch (
                    PathMoveStep nextLocation::currentReversedPath,
                        openValves,
                        score,
                        unopenedValveFlow))
        Seq.concat [openStepIfApplicable; moveSteps]
    | _ -> failwithf "Failed to find site %s" currentPositionName
    // Basic min/max culling.
    // The best score from the previous round sets a lower bar on the result - no matter
    // where we go next, we can't possible get a lower score than the one we already have.
    // So we can ask: do any of the proposed candidates have an upper bound on their score
    // that means they can't possibly produce a better score than the one we already know
    // we can achieve. A crude but simple calculation of the upper bound is to multiply the
    // combined flow of all as-yet-unopened valves by the amount of remaining time, and
    // adding that to our score so far. That's the score we'd get if we were magically able
    // to open all remaining closed valves right now.
    // (We could do more subtle calculations, based on how far away each of the valves is
    // from here. That might be necessary if this doesn't cull enough of the search space.)
    |> Seq.filter (fun (LocationInBreadthSearch (_, _, score, unopenedValveFlow)) ->
        let scoreFromOpeningRemainingValves = unopenedValveFlow * minutesRemaining
        let scoreUpperBound = score + scoreFromOpeningRemainingValves
        scoreUpperBound >= bestScoreFromLastRound)

candidateNextLocations testSummarized 0 testInitialLocation |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
        LocationInBreadthSearch ([PathMoveStep "DD"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
        LocationInBreadthSearch ([PathMoveStep "II"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
    ]
candidateNextLocations
    testSummarized
    0
    (LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81))
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (nodeSetFromLabel testNodeNames "BB"), 13 * 28, 81 - 13);
        LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
    ]

candidateNextLocations
    testSummarized
    (13 * 28)
    (LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (nodeSetFromLabel testNodeNames "BB"), 13 * 28, 81 - 13))
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathMoveStep "AA"; PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (nodeSetFromLabel testNodeNames "BB"), 13 * 28, 81 - 13);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (nodeSetFromLabel testNodeNames "BB"), 13 * 28, 81 - 13);
    ]

type BreadthSearchState =
    BreadthSearchState of currentLocations:(LocationInBreadthSearch list) *
    maximaByLocationThenValves:Map<string, (NodeSet * int) list>
let getInitialBreadthSearchState network =
    let initialLocation = getInitialLocation network
    let (SummarizedNetwork (_, nodeNames)) = network
    BreadthSearchState ([initialLocation], Map.empty |> Map.add "AA" [(emptyNodeSet nodeNames, 0)])
let testInitialBreadthSearchState = getInitialBreadthSearchState testSummarized
let inputInitialBreadthSearchState = getInitialBreadthSearchState summarized

type CandidateComparisonResult = LeftIsBetter | RightIsBetter | LeftAndRightIdentical
type PartialOrderComparisonResult = PartialOrdered of result:CandidateComparisonResult | NeitherIsEvidentlyBetter
let isBetterCandidate (leftPathLength:int) (leftOpenValves:NodeSet) (leftScore:int) rightPathLength (rightOpenValves:NodeSet) rightScore =
    let valves =
        if leftOpenValves.Members = rightOpenValves.Members then PartialOrdered LeftAndRightIdentical
        // Having more valves open isn't actually necessarily better. If two paths lead to the
        // same position and the same score, but one has fewer valves open, it has more scope
        // to improve its score by opening those other valves.
        //else if (Set.isSubset leftOpenValves rightOpenValves) then PartialOrdered RightIsBetter
        //else if (Set.isSubset rightOpenValves leftOpenValves) then PartialOrdered LeftIsBetter
        else NeitherIsEvidentlyBetter

    let compareScores (left:int) (right:int) =
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
        

// Shorter path, same effect (no valves yet)
// (AA,BB,CC,DD):0 vs (AA,DD):0
isBetterCandidate 4 testEmptyNodeSet 0 2 testEmptyNodeSet 0 =! PartialOrdered RightIsBetter
isBetterCandidate 2 testEmptyNodeSet 0 4 testEmptyNodeSet 0 =! PartialOrdered LeftIsBetter

// Shorter path, same effect


// Same path length, same valves, same score
// (AA,BB,O,CC,DD)-(BB):364 vs (AA,BB,O,AA,DD)-(BB):364
isBetterCandidate 5 (nodeSetFromLabels testNodeNames ["BB"]) 364 5 (nodeSetFromLabels testNodeNames ["BB"]) 364 =! PartialOrdered LeftAndRightIdentical

// Same path length, same valves, different score
// (AA,BB,CC,O,DD,O)-(CC,DD):554 vs (AA,DD,O,CC,O,DD):612
isBetterCandidate 6 (nodeSetFromLabels testNodeNames ["CC";"DD"]) 554 6 (nodeSetFromLabels testNodeNames ["CC";"DD"]) 612 =! PartialOrdered RightIsBetter
isBetterCandidate 6 (nodeSetFromLabels testNodeNames ["CC";"DD"]) 612 6 (nodeSetFromLabels testNodeNames ["CC";"DD"]) 554 =! PartialOrdered LeftIsBetter

// Same path length, different non-subset valves (different score, but doesn't matter)
// (AA,BB,O,AA,DD,O)-(BB,DD):864 vs (AA,BB,O,CC,O,DD)-(BB,CC):416
isBetterCandidate 6 (nodeSetFromLabels testNodeNames ["BB";"DD"]) 864 6 (nodeSetFromLabels testNodeNames ["BB";"CC"]) 416 =! NeitherIsEvidentlyBetter
isBetterCandidate 6 (nodeSetFromLabels testNodeNames ["BB";"CC"]) 416 6 (nodeSetFromLabels testNodeNames ["BB";"DD"]) 864 =! NeitherIsEvidentlyBetter

// Test valve subsets?

let filterCandidateLocationBasedOnState
    (BreadthSearchState (_, maximaByLocationAndValves))
    (LocationInBreadthSearch (candidateReversedPath, candidateOpenValves, candidateScore, unopenedValveFlow)) =
    let candidateLocationName = currentPositionNameFromReversedPath candidateReversedPath
    match maximaByLocationAndValves |> Map.tryFind candidateLocationName with
    | Some maximaByValves ->
        let atLeastOneEquivalentOrBetterExists =
            maximaByValves
            |> List.exists (fun (existingPositionOpenValves, existingPositionScore) ->
                (nodeSetContains candidateOpenValves existingPositionOpenValves) &&
                    existingPositionScore >= candidateScore)
        not atLeastOneEquivalentOrBetterExists
    | None -> true

candidateNextLocations
    testSummarized
    0
    (LocationInBreadthSearch ([PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81))
    |> Seq.filter (filterCandidateLocationBasedOnState testInitialBreadthSearchState)
    |> List.ofSeq =!
    [
        LocationInBreadthSearch ([PathOpenStep; PathMoveStep "BB"; PathMoveStep "AA"], (nodeSetFromLabel testNodeNames "BB"), 13*28, 81 - 13);
        LocationInBreadthSearch ([PathMoveStep "CC"; PathMoveStep "BB"; PathMoveStep "AA"], testEmptyNodeSet, 0, 81);
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
                let (LocationInBreadthSearch (reversedPath, openValves, score, unopenedValveFlow)) = location
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
                                    let (LocationInBreadthSearch (firstPath, firstOpenValves, firstScore, firstUnopenedValveFlow)) = firstCandidate
                                    let (LocationInBreadthSearch (secondPath, secondOpenValves, secondScore, secondUnopenedValveFlow)) = secondCandidate
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
    [LocationInBreadthSearch ([PathMoveStep "AA"], testEmptyNodeSet, 0, 81)] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"], testEmptyNodeSet, 0, 81)]

reduceCandidatesToSingleBest
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], nodeSetFromLabels testNodeNames ["CC"], 54, 81 - 2);
     LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], nodeSetFromLabels testNodeNames ["CC"], 54, 81 - 2)] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "BB"], nodeSetFromLabels testNodeNames ["CC"], 54, 81 - 2)]

reduceCandidatesToSingleBest
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "BB"; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"; PathOpenStep], nodeSetFromLabels testNodeNames ["CC";"DD"], 554, 81 - - 20);
     LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathOpenStep; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"], nodeSetFromLabels testNodeNames ["CC";"DD"], 612, 81 - 2 - 20)
     ] |> List.ofSeq
    =!
    [LocationInBreadthSearch ([PathMoveStep "AA"; PathMoveStep "DD"; PathOpenStep; PathMoveStep "CC"; PathOpenStep; PathMoveStep "DD"], nodeSetFromLabels testNodeNames ["CC";"DD"], 612, 81 - 2 - 20)]


let walkNetwork network =
    let initialBreadthSearchState = getInitialBreadthSearchState network
    Seq.unfold
        (fun state ->
            let (BreadthSearchState (currentLocations, currentMaxima)) = state
            let bestScoreFromLastRound =
                currentLocations
                |> Seq.map (fun (LocationInBreadthSearch (_, _, score, _)) -> score)
                |> Seq.max
            let candidates =
                currentLocations
                |> Seq.collect (candidateNextLocations network bestScoreFromLastRound)
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
                        let (LocationInBreadthSearch (currentReversedPath, openValves, score, unopenedValveFlow)) = location 
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

let displayLocation nodeNames (LocationInBreadthSearch (reversedPath, openValves, score, unopenedValveFlow)) =
    sprintf
        "%d(%d): %s-(%A)"
        score
        unopenedValveFlow
        (System.String.Join(", ", (Seq.rev reversedPath |> Seq.map displayPathStep)))
        (labelsFromNodeSet nodeNames openValves)

let solvePart1 summarized =
    let mutable result = 0
    let (SummarizedNetwork (_, nodeNames)) = summarized
    for locations in ((walkNetwork summarized) |> Seq.take 30) do
        result <- (locations |> Seq.map (fun (LocationInBreadthSearch (_,_,score, _)) -> score) |> Seq.max)
        //let sortedLocations =
        //    locations
        //    |> Seq.sortBy
        //        (fun (LocationInBreadthSearch (reversedPath, _,_,_)) ->
        //            Seq.rev reversedPath
        //            |> Seq.map (fun p ->
        //                match p with
        //                | PathOpenStep -> " "
        //                | PathMoveStep l -> l)
        //            |> List.ofSeq)
        //for location in sortedLocations do
        //    printf "%s\n" (displayLocation location)
        printf "Max: %d\n" result
        printf "  %A\n\n" (locations |> Seq.maxBy (fun (LocationInBreadthSearch (_,_,score, _)) -> score) |> displayLocation nodeNames)
    result

solvePart1 testSummarized =! 1651
solvePart1 summarized =! 2330

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
