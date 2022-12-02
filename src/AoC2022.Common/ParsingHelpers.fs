module ParsingHelpers

open FParsec

let testp (p : Parser<'TResult, unit>) s =
    match run p s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwithf "Parse failed: %s" error
