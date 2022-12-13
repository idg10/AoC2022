open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

open Checked

let input = getEmbeddedInput ()

let testInput = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

type PacketEntry =
    PacketItemSingle of value:int
    | PacketItemNested of items:PacketEntry list


let (parsePacketItems, parsePacketItemsR) = createParserForwardedToRef ()

let parsePacketItem =
    (pint32 |>> PacketItemSingle)
    <|>
    (parsePacketItems |>> PacketItemNested)
parsePacketItemsR :=
    pchar '[' >>. (sepBy parsePacketItem (pchar ',')) .>> pchar ']'

testp parsePacketItems "[]" =! []
testp parsePacketItems "[1,1,3,1,1]" =! [PacketItemSingle 1; PacketItemSingle 1; PacketItemSingle 3; PacketItemSingle 1; PacketItemSingle 1]
testp parsePacketItems "[1,1,3,1,1]" =! [PacketItemSingle 1; PacketItemSingle 1; PacketItemSingle 3; PacketItemSingle 1; PacketItemSingle 1]
testp parsePacketItems "[[1],[2,3,4]]" =!
    [
        PacketItemNested [PacketItemSingle 1];
        PacketItemNested [PacketItemSingle 2; PacketItemSingle 3; PacketItemSingle 4]
    ]
testp parsePacketItems "[1,[2,[3,[4,[5,6,7]]]],8,9]" =!
    [
        PacketItemSingle 1;
        PacketItemNested [
            PacketItemSingle 2;
            PacketItemNested [
                PacketItemSingle 3;
                PacketItemNested [
                    PacketItemSingle 4;
                    PacketItemNested [PacketItemSingle 5; PacketItemSingle 6; PacketItemSingle 7;]
                ]
            ]
        ];
        PacketItemSingle 8;
        PacketItemSingle 9;
    ]

let parsePair = //<'a> : Parser<PacketEntry list * PacketEntry list, 'a> =
    pipe2 (parsePacketItems .>> newline) parsePacketItems (fun a b -> (a,b))

testp parsePair """[9]
[[8,7,6]]""" =! ([PacketItemSingle 9], [PacketItemNested [PacketItemSingle 8; PacketItemSingle 7; PacketItemSingle 6]])

let parseInputAsPairs = sepBy parsePair (newline >>. newline)
let testPairs = testp parseInputAsPairs testInput
let pairs = testp parseInputAsPairs input
let parseInputAsPackets input =
    let listFromInput = (testp (sepBy parsePacketItems (many1 newline)) input)
    List.concat [
        listFromInput;
        [[PacketItemNested [PacketItemSingle 2]]; [PacketItemNested [PacketItemSingle 6]]]
    ]
let testPackets = parseInputAsPackets testInput
let packets = parseInputAsPackets input

let rec comparePacketEntry (left: PacketEntry) (right: PacketEntry) =
    match left with
    | PacketItemSingle leftValue ->
         match right with
         | PacketItemSingle rightValue -> 
            if leftValue < rightValue then Some true
            else if leftValue > rightValue then Some false
            else None
         | PacketItemNested _ -> comparePacketEntry (PacketItemNested [left]) right
    | PacketItemNested leftValues ->
        match right with
        | PacketItemSingle _ -> comparePacketEntry left (PacketItemNested [right])
        | PacketItemNested rightValues ->
            Seq.unfold
                // We're using unfold to iterate over the two lists, because zip doesn't provide
                // an easy way to see when we're at the end.
                // Our state is the two lists (which we consume the heads of each time around).
                // Our output is an option, where we return None if we're still going. The
                // generator produces Some Some bool as soon as it is done. Note that it has
                // to be a doubly-wrapped option because there are four options:
                //  None (more processing to do)
                //  Some None (processing done, and no differences detected)
                //  Some false (definitely in the wrong order)
                //  Some true (definitely in the right order)
                //, and it makes
                // no attempt to stop, so it's important that we attempt only to consume
                // the first non-None item that comes out.
                (fun (left, right) ->
                    match left with
                    | [] ->
                        // If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
                        if (right = []) then Some (Some None, ([], []))
                        // If the left list runs out of items first, the inputs are in the right order.
                        else Some (Some (Some true), ([], []))
                    | leftFirst::leftRest ->
                        match right with
                        // If the right list runs out of items first, the inputs are not in the right order.
                        | [] -> Some (Some (Some false), ([], []))
                        | rightFirst::rightRest ->
                            match comparePacketEntry leftFirst rightFirst with
                            | Some result -> Some (Some (Some result), ([], []))
                            | None -> Some (None, (leftRest, rightRest)))
                (leftValues, rightValues)
            |> Seq.choose id
            |> Seq.head

comparePacketEntry (PacketItemSingle 1) (PacketItemSingle 2) =! Some true
comparePacketEntry (PacketItemSingle 2) (PacketItemSingle 1) =! Some false
comparePacketEntry (PacketItemSingle 1) (PacketItemSingle 1) =! None

let comparePair (left, right) = comparePacketEntry (PacketItemNested left) (PacketItemNested right)

comparePair (testp parsePair """[1,1,3,1,1]
[1,1,5,1,1]""") =! Some true

comparePair (testp parsePair """[[1],[2,3,4]]
[[1],4]""") =! Some true

comparePair (testp parsePair """[9]
[[8,7,6]]""") =! Some false

comparePair (testp parsePair """[[4,4],4,4]
[[4,4],4,4,4]""") =! Some true

comparePair (testp parsePair """[7,7,7,7]
[7,7,7]""") =! Some false

comparePair (testp parsePair """[]
[3]""") =! Some true

comparePair (testp parsePair """[[[]]]
[[]]""") =! Some false

comparePair (testp parsePair """[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]""") =! Some false

let getIndicesOfPairsInRightOrder pairs =
    pairs
    |> Seq.mapi (fun i p -> if (comparePair p) = (Some true) then Some (i + 1) else None)
    |> Seq.choose id

getIndicesOfPairsInRightOrder testPairs |> List.ofSeq =! [1; 2; 4; 6]

let getSumOfRightlyOrderedPairIndices pairs = getIndicesOfPairsInRightOrder pairs |> Seq.sum

getSumOfRightlyOrderedPairIndices testPairs =! 13

printf "Part 1: %d\n" (getSumOfRightlyOrderedPairIndices pairs)

let sortPackets packets =
    packets
    |> Seq.sortWith
        (fun left right ->
            match comparePair(left, right) with
            | Some true -> -1
            | Some false -> 1
            | None -> 0)

//printf "%A\n" (sortPackets testPackets |> List.ofSeq)

let getDividerIndices packets =
    sortPackets packets
    |> Seq.mapi (fun i p -> (i + 1, p))
    |> Seq.fold
        (fun (idxOf2Marker, idxOf6Marker) (i, p) ->
            match p with
            | [PacketItemNested [PacketItemSingle 2]] -> (Some i, idxOf6Marker)
            | [PacketItemNested [PacketItemSingle 6]] -> (idxOf2Marker, Some i)
            | _ -> (idxOf2Marker, idxOf6Marker))
        (None, None)

let getDecoderKey packets =
    match getDividerIndices packets with
    | (Some m2i, Some m6i) -> m2i * m6i
    | _ -> failwith "Did not find all markers"

//printf "%A\n" (getDividerIndices testPackets)
getDecoderKey testPackets =! 140

printf "Part 2: %d\n" (getDecoderKey packets)
