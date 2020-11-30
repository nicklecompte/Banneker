module CombinatorialTypeSystem.BasicTypes

open Name
open Nat


type Sort =
    | Type
    | Kind of Nat

let sortToName sort =
    match sort with
    | Type -> UserName "Type"
    | Kind n -> UserName (sprintf "Kind_%i" (Nat.toInt n))