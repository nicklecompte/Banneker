module Scheme

open BuiltinPrimitives

type SchemeExpression =
    | Constant of string
    | Lambda of arg:SchemeExpression*body:SchemeExpression
    | Application of SchemeExpression*SchemeExpression
    | ExpList of List<SchemeExpression>
    // Type annotations, etc are erased.
    | Erased
with
    override x.ToString() =
        match x with
        | Constant s -> s
        | Lambda (arg,body) -> 
            let argS = arg.ToString()
            let bodyS = body.ToString()
            sprintf "lambda %s %s" argS bodyS
        | Application (a,b) -> sprintf "(%s %s)" (a.ToString()) (b.ToString())
        | ExpList xs ->
            let stringList = List.map (fun y -> y.ToString()) xs
            let builder = System.Text.StringBuilder()
            builder.Append("( ") |> ignore
            for x in stringList do
                builder.Append(' ') |> ignore
                builder.Append(x.ToString()) |> ignore
            builder.Append(')') |> ignore
            builder.ToString()

        | Erased -> ""

let literalToSchemeExpression (lit:Literal) =
    match lit with
    | Int i -> Constant (i.ToString())
    | Char c -> Constant (sprintf "#\\%c" c)
    | String s -> Constant (sprintf "\"%s\"" s)
    | Double d -> Constant (d.ToString())
    | Bool b -> Constant (match b with | true -> "#t" | false -> "#f")
    | _ -> Erased

let primitiveFunctionToSchemeExpression (f:PrimitiveFunction) (args: Literal list) : SchemeExpression =
    match args with
    | [Int a; Int b] ->
        let consExpression = 
            Application ((literalToSchemeExpression (Int a)), (literalToSchemeExpression (Int b)))
        match f with
        | Add IntType -> Application ((Constant "+"),consExpression)
        | Sub IntType -> Application ((Constant "-"),consExpression)
        | Mul IntType -> Application ((Constant "*"),consExpression)
        | Div IntType -> Application ((Constant "/"),consExpression)
        | LT IntType -> Application ((Constant "<"),consExpression)
        | LTE IntType -> Application ((Constant "<="),consExpression)
        | GT IntType -> Application ((Constant ">"),consExpression)
        | GTE IntType -> Application ((Constant "<="),consExpression)
        | _ -> failwithf "unsupported primiitive function for 2 integer arguments: %A" f
    | [Int a] ->
        let endExp = Constant (a.ToString())
        match f with
        | Neg IntType -> Application ((Constant "-"),endExp)
    // match (f,args) with
    // | (Add IntType,[Int a; Int b]) -> 
    //     Application 
    //         (Constant "+") 
    // | (Sub IntType, [Int a; Int b]) -> Int (a - b)
    // | (Mul IntType, [Int a; Int b]) -> Int (a * b)
    // | (Div IntType, [Int a; Int b]) -> Int (a - b)
    // | (Mod IntType, [Int a; Int b]) -> Int (a % b)
    // | (Neg IntType, [Int a]) -> Int (-a)
    // | (LT IntType, [Int a; Int b]) -> Bool (a < b)
    // | (LTE IntType, [Int a; Int b]) -> Bool (a <= b)
    // | (GT IntType, [Int a; Int b]) -> Bool (a > b)
    // | (GTE IntType, [Int a; Int b]) -> Bool (a >= b)

    // | (Cast (IntType, DoubleType), [Int a]) -> Double (double(a))
    // | _ -> 
    //     raise (ArgumentException("executePrimitiveFunction got bad argument count"))