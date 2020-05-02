
open System
open System.IO

type Pos = {line:int; col:int}
let Pos line col = {line = line; col = col}

type Token =
    | ADD   of Pos
    | SUB   of Pos
    | GE    of Pos
    | LPAR  of Pos
    | RPAR  of Pos
    | EQL   of Pos
    | DOT   of Pos
    | WHILE of Pos
    | INT   of Pos * int
    | FLOAT of Pos * float
    | ID    of Pos * string
    | ERROR of Pos * string

type TokenMap = Map<char list, Pos -> Token>

let toTokenMap : (string * (Pos -> Token)) list -> TokenMap = List.map (fun (a, b) -> (Seq.toList a, b)) >> Map.ofList

let keys m = m |> Map.toList |> List.map fst

let symbols : TokenMap = toTokenMap ["+",ADD; "-",SUB; ">=",GE; "(",LPAR; ")",RPAR; "=",EQL; ".",DOT]

let keywords : TokenMap = toTokenMap ["while",WHILE]

let rec accumulate (f: char -> bool) (result: char list) (str: char list) : char list * char list =
    match str with
    | [] -> (result, [])
    | c::cs when f c -> accumulate f (result@[c]) cs
    | _ -> (result, str)

let inline (|DigitStart|SymbolStart|IDStart|InvalidStart|) c =
    match c with
    | c when Char.IsDigit c -> DigitStart
    | c when List.contains c (symbols |> keys |> List.map Seq.head) -> SymbolStart
    | c when List.contains c <| ['_']@['a'..'z']@['A'..'Z'] -> IDStart
    | _ -> InvalidStart

let rec lex' sl col ln =
    match sl with
    | [] -> []
    | []::xs -> lex' xs 1 (ln+1)
    | (c::cs)::xs ->
        let lexFn = lexMulti c cs xs col ln
        match c with
        | DigitStart -> lexNum c cs xs col ln
        | SymbolStart -> lexFn symbols (fun x -> List.contains x (symbols |> keys |> List.reduce (@))) ERROR
        | IDStart -> lexFn keywords (fun x -> List.contains x <| ['_']@['a'..'z']@['A'..'Z']@['0'..'9']) ID
        | _ -> lex' (cs::xs) (col+1) ln

and lexNum c cs xs col ln =
    let read f lst = lst |> List.toArray |> String |> f
    let value, cs' = accumulate (fun x -> Char.IsDigit x || x = '.') [c] cs
    let recurseFn = lex' (cs'::xs) (col + value.Length) ln
    match List.contains '.' value with
    | true -> (FLOAT <| (Pos ln col, value |> read float)) :: recurseFn
    | false -> (INT <| (Pos ln col, value |> read int)) :: recurseFn
    
and lexMulti c cs xs col ln tokenMap accFn elseFn =
    let value, cs' = accumulate accFn [c] cs
    let recurseFn = lex' (cs'::xs) (col + value.Length) ln
    match Map.tryFind value tokenMap with
    | Some tokenFn -> (tokenFn (Pos ln col)) :: recurseFn
    | None -> (elseFn (Pos ln col, value |> Array.ofList |> String)) :: recurseFn

let lex (str: string) : Token list = 
    lex' (str.Split('\n') |> List.ofArray |> List.map Seq.toList) 1 1

[<EntryPoint>]
let main argv =
    File.ReadAllText "test.mlang" |> lex |> List.iter (fun t -> (string t).Replace("\n","").Replace("      ","  ") |> printfn "%s")
    0
