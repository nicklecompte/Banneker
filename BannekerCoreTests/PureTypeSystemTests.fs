module PureTypeSystemTests

open Xunit
open Banneker.PureTypeSystem

[<Fact>]
let ``alpha-equivalent sorts smoke test`` () =
    let sort1 = Sort "type"
    let sort1a = Sort "type"
    let sort2 = Sort "kind"
    Assert.True(alphaEquivalentTerms (SortTerm sort1) (SortTerm sort1a))
    Assert.False(alphaEquivalentTerms (SortTerm sort1) (SortTerm sort2))

[<Fact>]
let ``alpha-equivalent variables smoke test`` () =
    let nameType = UserName "Nat"
    let natType = TypedVar(nameType,SortTerm(Sort "Type"))
    let name1 = UserName "myNat"
    let var1 = Variable (TypedVar(name1,Variable natType))
    let var1A = Variable(TypedVar(name1,Variable natType))
    let name2 = UserName "myOtherNat"
    let var2 = Variable(TypedVar(name2,Variable natType))

    Assert.True(alphaEquivalentTerms var1 var1A)
    Assert.False(alphaEquivalentTerms var1 var2)