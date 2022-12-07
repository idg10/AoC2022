open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let testInputText = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

// Note: not splitting into rows this time because the input comes in multiple sections
// and we're going to express that through a top-level parser
let inputText = getEmbeddedInput ()

type CommandLs = CommandLs
type CommandCd = CommandCd of dir:string
type LsOutputLine =
    LsDir of path:string
    | LsFile of size:int * path:string    

let pCmdLs<'a> : Parser<CommandLs, 'a> =
    pstring "$ ls" |>> fun _ -> CommandLs

let pathItemChar<'a> : Parser<char, 'a> = satisfy (fun c -> c = '.' || System.Char.IsLetter c)
let pFolderName<'a> : Parser<string, 'a> =
    pstring ".." <|> pstring "/" <|> many1Chars pathItemChar
let pFileName<'a> : Parser<string, 'a> = many1Chars pathItemChar

let pCmdCd<'a> : Parser<CommandCd, 'a> =
    (pstring "$ cd" >>. spaces >>. pFolderName) |>> CommandCd

testp pCmdLs "$ ls" =! CommandLs
testp pCmdCd "$ cd x" =! CommandCd "x"
testp pCmdCd "$ cd .." =! CommandCd ".."

let pDirOutput<'a> : Parser<LsOutputLine, 'a> =
    pstring "dir" >>. spaces >>. pFolderName
    |>> LsDir

let pFileOutput<'a> : Parser<LsOutputLine, 'a> =
    pipe2
        pint32
        (spaces >>. pFileName)
        (fun size name -> LsFile (size, name))

testp pFileOutput "123 abc" =! LsFile (123, "abc")
testp pFileOutput "4321 file.txt" =! LsFile (4321, "file.txt")
testp pDirOutput "dir xyz" =! LsDir "xyz"



type ConsoleOutputItem =
    ConsoleOutputCdItem of dir:string
    | ConsoleOutputLsItem of lines:LsOutputLine list
type ConsoleOutput = ConsoleOutputItem list

let pParseConsoleCdOutputItem<'a> : Parser<ConsoleOutputItem, 'a> =
    pCmdCd |>> (fun (CommandCd path) -> ConsoleOutputCdItem path)

// Note use of >>? here because the newline at the end of each individual output of the
// ls command is ambiguous: it might be a separator between two output lines, or it might
// denote the end of the final line of output for this command. We're only going to know
// by looking at the following line and seeing if it parses as another ls output line.
// We're using >>? because that will backtrack if its right hand side fails. (Using
// "newline >>. " would consume any newline, and then just fail completely if the
// RHS turns out not to be an ls command output line.)
let pParseConsoleOutputLsItem<'a> : Parser<ConsoleOutputItem, 'a> =
    pCmdLs >>. (many1 (newline >>? (pDirOutput <|> pFileOutput)))
    |>> ConsoleOutputLsItem

testp pParseConsoleOutputLsItem """$ ls
dir a
1234 file.txt""" =! ConsoleOutputLsItem [LsDir "a"; LsFile (1234, "file.txt")]

let pParseConsoleOutputItem<'a> : Parser<ConsoleOutputItem, 'a> =
    pParseConsoleCdOutputItem <|> pParseConsoleOutputLsItem

testp pParseConsoleOutputItem """$ ls
dir a
1234 file.txt""" =! ConsoleOutputLsItem [LsDir "a"; LsFile (1234, "file.txt")]
testp pParseConsoleCdOutputItem "$ cd a" =! ConsoleOutputCdItem "a"

let pParseConsoleOutput<'a> : Parser<ConsoleOutput, 'a> =
    sepBy pParseConsoleOutputItem newline

testp pParseConsoleOutput testInputText =! [
    ConsoleOutputCdItem "/";
    (ConsoleOutputLsItem
        [
            LsDir "a";
            LsFile (14848514, "b.txt");
            LsFile (8504156, "c.dat");
            LsDir "d"
        ])
    ConsoleOutputCdItem "a";
    (ConsoleOutputLsItem
        [
            LsDir "e";
            LsFile (29116, "f");
            LsFile (2557, "g");
            LsFile (62596, "h.lst");
        ])
    ConsoleOutputCdItem "e";
    (ConsoleOutputLsItem
        [
            LsFile (584, "i");
        ])
    ConsoleOutputCdItem "..";
    ConsoleOutputCdItem "..";
    ConsoleOutputCdItem "d";
    (ConsoleOutputLsItem
        [
            LsFile (4060174, "j");
            LsFile (8033020, "d.log");
            LsFile (5626152, "d.ext");
            LsFile (7214296, "k")
        ])
]

type FsEntry = FsFile of size:int * name:string | FsDirectory of name:string * contents:FsEntry list

// This works through each chunk of output, which will either be a "cd" command or an "ls" command with its output.
// Our state is a stack keeping track of the current directory. As we "cd" into a folder, we add an entry to the
// stack. As we "cd .." back out of it, we pop that from the stack, and add an FsDirectory into the parent folder's
// items.
let buildFsFromConsoleOutput (output:ConsoleOutput) =
    let folder (stateStack:FsEntry list) (item:ConsoleOutputItem) =
        match stateStack with
        | [] -> failwith "State stack should not end up empty"
        | fsEntry::dirStack ->
            match fsEntry with
            | FsFile _ -> failwith "Should not end up with FsFile entries on the state stack"
            | FsDirectory (currentDirName, currentDirContents) ->
                match item with
                | ConsoleOutputCdItem name ->
                    match name with
                    | ".." ->
                        // Now we're coming out of the current directory, we need to pop it off
                        // the state stack and merge it in as a child of the parent directory
                        match dirStack with
                        | FsDirectory (parentDirName, parentDirExistingContents)::remainingDirs ->
                            let newChildDir = FsDirectory (currentDirName, currentDirContents)
                            FsDirectory (parentDirName, newChildDir::parentDirExistingContents)::remainingDirs
                        | _ -> failwithf "Unexpected state stack %A" dirStack
                    | _ ->
                        // We're entering a new subdirectory, so we need to 
                        (FsDirectory (name, []))::(FsDirectory (currentDirName, currentDirContents))::dirStack
                | ConsoleOutputLsItem lines ->
                    let dirItems =
                        lines
                        |> List.map (fun line ->
                            match line with
                            | LsFile (size, name) -> Some (FsFile (size, name))
                            | LsDir name -> None) // We'll add the FsDirectory as we "cd .." our way out
                        |> List.choose id // Filter out the None entries that the LsDir entries produced
                    (FsDirectory (currentDirName, dirItems))::dirStack

    let finalDirStack = output |> List.fold folder ([FsDirectory ("", [])])
    
    // At this point, if the output didn't "cd .." its way to the top, our state stack will have
    // entries for all the folders in the path to the current directory. We need to collapse these
    // back out to get the final representation of the root folder.
    let rootDirStack =
        [1..(finalDirStack.Length - 1)]
        |> List.map (fun _ -> ConsoleOutputCdItem "..")
        |> List.fold folder finalDirStack
    
    let rootDir = List.exactlyOne rootDirStack

    // One last wrinkle: we initialize our state stack with an FsDirectory, and the first "cd /" ends up being
    // a child of this. In principle there might be other roots, but in practice we want "/" to be the root, so
    // strip that off
    let slashFolder =
        match rootDir with
        | FsDirectory ("", entries) -> List.exactlyOne entries
        | _ -> failwithf "Unexpected state: %A" rootDir
    slashFolder

let testRoot = (buildFsFromConsoleOutput <| testp pParseConsoleOutput testInputText)
let root = (buildFsFromConsoleOutput <| testp pParseConsoleOutput inputText)

testRoot =! FsDirectory (
    "/",
    [
        FsDirectory (
           "d",
        [
            FsFile (4060174, "j");
            FsFile (8033020, "d.log");
            FsFile (5626152, "d.ext");
            FsFile (7214296, "k")
        ]);
        FsDirectory (
           "a",
        [
            FsDirectory (
                "e",
                [ FsFile (584, "i")]
            );
            FsFile (29116, "f");
            FsFile (2557, "g");
            FsFile (62596, "h.lst")
        ]);
        FsFile (14848514, "b.txt");
        FsFile (8504156, "c.dat");
    ])

let rec fsEntrySizes:(FsEntry -> (((string * bigint) list) * bigint)) = fun dir ->
    match dir with
    | FsDirectory (dirName, dirContents) ->
        let (descendantSizes, dirSize) =
            dirContents
            |> List.fold
                (fun (descendantSizesAcc, totalFileSizeThisFolderAndDecendants) item ->
                    match item with
                    | FsFile (fileSize, _) -> (descendantSizesAcc, totalFileSizeThisFolderAndDecendants + (bigint fileSize))
                    | FsDirectory (childDirName, childDirContents) ->
                        let (childDirSizes, dirSize) = fsEntrySizes (FsDirectory (childDirName, childDirContents))
                        //let combinedMap = Map.fold (fun m key value -> Map.add key value m) map dirMap
                        let combinedSizes = List.concat [descendantSizesAcc; childDirSizes]
                        (combinedSizes, totalFileSizeThisFolderAndDecendants + dirSize)
                )
                ([], bigint 0)
        ((dirName, dirSize)::descendantSizes, dirSize)
    | _ -> failwith "Can only use this on directories"


let testSizeList = fsEntrySizes testRoot |> fst
let inputSizeList = fsEntrySizes root |> fst

//printf "%A\n" testSizeList
List.find (fun (n, s) -> n = "e") testSizeList |> snd =! bigint 584
List.find (fun (n, s) -> n = "a") testSizeList |> snd =! bigint 94853
List.find (fun (n, s) -> n = "d") testSizeList |> snd =! bigint 24933642
List.find (fun (n, s) -> n = "/") testSizeList |> snd =! bigint 48381165

let directoriesNoLargerThan max dirs =
    dirs
    |> Seq.filter (fun (_, size) -> size <= max)

let dirsForPart1 dirs = directoriesNoLargerThan (bigint 100000) dirs
let sumForPart1 dirs = dirsForPart1 dirs |> Seq.sumBy snd

sumForPart1 testSizeList =! bigint 95437

//printf "%A\n" root
//printf "%A\n" (dirsForPart1 inputSizeList |> List.ofSeq)

let part1map = sumForPart1 inputSizeList
printf "Part 1: %A\n" part1map


let testInputText2 = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd a
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

let testRoot2 = (buildFsFromConsoleOutput <| testp pParseConsoleOutput testInputText2)
//printf "%A\n" testRoot2

let findFreeSpace (sizeList:(string * bigint) list) =
    let usedSpace = List.find (fun (n, s) -> n = "/") sizeList |> snd
    (bigint 70000000) - usedSpace

findFreeSpace testSizeList =! bigint 21618835

let findFoldersLargeEnoughToFreeUpSpace sizeList =
    let freeSpace = findFreeSpace sizeList
    let additionalSpaceRequired = (bigint 30000000) - freeSpace
    sizeList |> Seq.filter (fun (_, size) -> size >= additionalSpaceRequired)

findFoldersLargeEnoughToFreeUpSpace testSizeList |> List.ofSeq =! [("/", bigint 48381165); ("d", bigint 24933642)]

let findBestFolderToFreeUp (sizeList:(string*bigint) list) =
    findFoldersLargeEnoughToFreeUpSpace sizeList
    |> Seq.sortBy snd
    |> Seq.take 1
    |> Seq.exactlyOne

findBestFolderToFreeUp testSizeList =! ("d", bigint 24933642)

printf "Part 2: %A\n" (findBestFolderToFreeUp inputSizeList)
