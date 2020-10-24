module Banneker.Primitives

open System

[<Struct>]
type Literal =
    | Int of intval:int
    | Char of charval:char
    | String of strptr:string
    | Double of dbval:double
    | Bool of bval:bool
    | IntType
    | CharType
    | StringType
    | DoubleType

let isVal lit =
    match lit with
    | Int _ -> true
    | Char _ -> true
    | String _ -> true
    | Double _ -> true
    | Bool _ -> true
    | _ -> false


type PrimitiveFunction =
    | Add of Literal
    | Sub of Literal
    | Mul of Literal
    | Div of Literal
    | Mod of Literal
    | Neg of Literal
    | LT of Literal
    | LTE of Literal
    | GT of Literal
    | GTE of Literal
    | Cast of Literal*Literal

let executePrimitiveFunction (f:PrimitiveFunction) (args: Literal list) : Literal =
    match (f,args) with
    | (Add IntType,[Int a; Int b]) -> Int (a + b)
    | (Sub IntType, [Int a; Int b]) -> Int (a - b)
    | (Mul IntType, [Int a; Int b]) -> Int (a * b)
    | (Div IntType, [Int a; Int b]) -> Int (a - b)
    | (Mod IntType, [Int a; Int b]) -> Int (a % b)
    | (Neg IntType, [Int a]) -> Int (-a)
    | (LT IntType, [Int a; Int b]) -> Bool (a < b)
    | (LTE IntType, [Int a; Int b]) -> Bool (a <= b)
    | (GT IntType, [Int a; Int b]) -> Bool (a > b)
    | (GTE IntType, [Int a; Int b]) -> Bool (a >= b)

    | (Cast (IntType, DoubleType), [Int a]) -> Double (double(a))
    | _ -> 
        raise (ArgumentException("executePrimitiveFunction got bad argument count"))


    //  StrLength : PrimFn 1
    //  StrHead : PrimFn 1
    //  StrTail : PrimFn 1
    //  StrIndex : PrimFn 2
    //  StrCons : PrimFn 2
    //  StrAppend : PrimFn 2
    //  StrReverse : PrimFn 1
    //  StrSubstr : PrimFn 3

    //  DoubleExp : PrimFn 1
    //  DoubleLog : PrimFn 1
    //  DoubleSin : PrimFn 1
    //  DoubleCos : PrimFn 1
    //  DoubleTan : PrimFn 1
    //  DoubleASin : PrimFn 1
    //  DoubleACos : PrimFn 1
    //  DoubleATan : PrimFn 1
    //  DoubleSqrt : PrimFn 1
    //  DoubleFloor : PrimFn 1
    //  DoubleCeiling : PrimFn 1

    //  Cast : Constant -> Constant -> PrimFn 1
    //  BelieveMe : PrimFn 3
    //  Crash : PrimFn 2