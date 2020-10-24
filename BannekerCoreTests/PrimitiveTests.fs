module Tests

open System
open Xunit
open Banneker.Primitives

[<Fact>]
let ``Smoke-test execPrimitiveFunction`` () =
    let testIntAdd = executePrimitiveFunction (Add IntType) [Int 2; Int 4]
    match testIntAdd with
        | Int a -> Assert.Equal(6,a)
        | _ -> raise (Failure(sprintf "Incorrect type for Add IntType PrimitiveFunction test: %s" (testIntAdd.ToString())))
