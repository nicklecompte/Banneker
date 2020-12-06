module Lexer

open Utils

type BannekerTokenizedResult =
    /// sorts = {Prop,Type} => SortNames ["Prop", "Type"]
    | SortNames of string list
    /// axioms = [{Prop : Type}] -> AxiomDefs [("Prop","Type")]
    | AxiomDefs of (string*string) list
    | RuleDefs of (string*string*string) list
    /// let x : Nat = S Z => LetExpr("x",Some "Nat",ListExpr(VarName "S"; VarName "Z")) 
    | LetExpr of varName:string*varType:(string option)*body:BannekerTokenizedResult
    | ModuleName of string
    | AndExpr of left:BannekerTokenizedResult*right:BannekerTokenizedResult
    | OrExpr of left:BannekerTokenizedResult*right:BannekerTokenizedResult
    | FunExpr of varName:string*varType:(string option)*varBody:BannekerTokenizedResult
    | VarName of string
    | Parenthesized of BannekerTokenizedResult
    | ListExpr of BannekerTokenizedResult list

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
    | Equals
    | Absurd
    | Interface
    | AutoDerive
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
    | NewLine
    | Whitespace
    | ErasedMark
    // TODO: Think about implementation of `trivial` attribute (%trivial?)
    // TODO: for lemmas/etc. The idea is to avoid developer pain with type errors
    // TODO: such as "xs ++ [] != xs" since you "forgot" appendRightNil rewrite.
    // TODO: Trivial would automatically apply these rewrites, then output what
    // TODO: steps it took, or throw an error if it couldn't find appropriate
    // TODO: rewrites in the context.
    | Trivial

type LexedBannekerElement =
    | NewLine
    | SignificantWhitespace
    | Comment of string
    | Variable of string
    | IfElt of cond:LexedBannekerElement * thenElt:LexedBannekerElement * elseElt:LexedBannekerElement
    // TODO: Unwrap tuples in lets, etc
    | LetElt of variableName:string*LexedBannekerElement


let stringToToken str =
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
    | "//" -> ValueSome BannekerToken.Comment
    | "(*" -> ValueSome BlockCommentStart
    | "*)" -> ValueSome BlockCommentEnd
    | "\n" -> ValueSome BannekerToken.NewLine
    | "    " -> ValueSome Whitespace
    | "\t" -> ValueSome Whitespace
    | "=" -> ValueSome Equals
    | _ -> ValueNone

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

type BannekerLexingResult =
    | SuccessfullyLexed of KeyedTree<BannekerToken,string>


// let rec splitBySemanticWhitespaceAndDelimiters (str:string) : SemanticallyDelimitedBlock list =
//     let splitByLines = str.Split('\n') |> List.ofArray
//     let withIndentationLevels = List.map (fun x -> (getIndentationLevel x, x)) splitByLines
//     let rec splitCore curlist curindent =
//         match curlist with
//         | [] -> []
//         | (curindent,x) :: [] -> [SingleBlock [x]]
//         | (curindent,x) :: (b,y) :: xs -> failwith "not done"

//     splitCore withIndentationLevels 0
    

// let toxenize (str:string) =
//     let blocks = splitBySemanticWhitespaceAndDelimiters str
//     match blocks with
//     | [SingleBlock xs] -> failwith "not done"
//     | NestedBlock(o,xs) :: ys -> failwith "not done"