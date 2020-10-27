module Lexer

type BannekerToken =
    | Let
    | Module
    | And
    | Or
    | Fun
    | Sort
    | Axiom
    | Rule
    | Rules
    | Forall
    | Exists
    | Absurd
    | Interface
    | With
    | Member
    | Match
    | MatchCaseMarker
    | Lparen
    | Rparen
    | If
    | Comment
    | BlockCommentStart
    | BlockCommentEnd

let stringToReservedTerm str =
    match str with
    | "let" -> ValueSome Let
    | "module" -> ValueSome Module
    | "and" -> ValueSome And
    | "or" -> ValueSome Or
    | "fun" -> ValueSome Fun
    | "sort" -> ValueSome Sort
    | "axiom" -> ValueSome Axiom
    | "rule" -> ValueSome Rule
    | "rules" -> ValueSome Rules
    | "forall" -> ValueSome Forall
    | "exists" -> ValueSome Exists
    | "absurd" -> ValueSome Absurd
    | "interface" -> ValueSome Interface
    | "with" -> ValueSome With
    | "member" -> ValueSome Member
    | "match" -> ValueSome Match
    | "(" -> ValueSome Lparen
    | ")" -> ValueSome Rparen
    | "if" -> ValueSome If
    | "//" -> ValueSome Comment
    | "(*" -> ValueSome BlockCommentStart
    | "*)" -> ValueSome BlockCommentEnd
    | _ -> 
        ValueNone

let rec getIndentationLevel (str:string) =
    if str.Length < 2 then 0
    else
        match str.[0] with
        | '\t' -> 1 + (getIndentationLevel str.[1..])
        | _ ->
            match str.Length >= 4 with
            | false -> 0
            | true -> 
                match str.[0..3] with
                | "    " -> 1 + (getIndentationLevel str.[4..])
                | _ -> 0

type SemanticallyDelimitedBlock =
    | SingleBlock of string list
    | NestedBlock of outer:SemanticallyDelimitedBlock * inner:List<SemanticallyDelimitedBlock>

let rec splitBySemanticWhitespaceAndDelimiters (str:string) : SemanticallyDelimitedBlock list =
    let splitByLines = str.Split('\n') |> List.ofArray
    let withIndentationLevels = List.map (fun x -> (getIndentationLevel x, x)) splitByLines
    let rec splitCore curlist curindent =
        match curlist with
        | [] -> []
        | (curindent,x) :: [] -> [SingleBlock [x]]
        | (curindent,x) :: (b,y) :: xs -> failwith "not done"

    splitCore withIndentationLevels 0
    

let toxenize (str:string) =
    let blocks = splitBySemanticWhitespaceAndDelimiters str
    match blocks with
    | [SingleBlock xs] -> failwith "not done"
    | NestedBlock(o,xs) :: ys -> failwith "not done"