module CombinatorialTypeSystem.Name

/// The name of an object in Banneker is a union Type, Name, with three constructors:
type Name =
    /// UserName is simply a string.
    /// Example: `let myVariable : Nat = S S Z` induces a UserName "myVariable"
    | UserName of string
    /// Sometimes the compiler will need to fill in names
    /// Example: User => let myAppend {xs : List a} (pf: SomeData xs) (ys : List a) : pf SomeData (xs ++ ys)
    /// 
    | MachineGenName of Name * int
    /// Imports from other Banneker modules are qualified with the name of the namespace 
    // Example: 
    // {
    // import ModuleWithQualfiedAccess
    // import ModuleWithPublicExports
    // from Module2WithQualifiedAccess import third_func
    // 
    // let myVar : MyType = ModuleWithQualfiedAccess.some_function myArgument
    // some_function has the name `NamespaceQualified ModuleWithQualfiedAccess some_function`
    // let mySecondVar : MySecondType = some_second_function mySecondArgument
    // some_seciond_function has the name `NamespaceQualfifed ModuleWithPublicExports some_second_function
    // let thirdVar : ThirdType = third_func thirdArg
    // third_func has name UserName "third_func"! How the from .. import .. works.
    // but lexer will decode that and it's not necessary for the user if the function is public and the module is "export all public"
    // }
    // TODO: Should this be in CombinatorialTypeSystem?
    | NamespaceQualified of ns:Name*namedObject:Name
