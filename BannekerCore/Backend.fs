/// !Docbuild
/// \subsection{The Banneker bootstrap compiler backend}
/// The Backend language is a very simple untyped functional programming language,
/// similar to Scheme.
module Backend

open BuiltinPrimitives
open CombinatorialTypeSystem
open Term
open BasicTypes

type BackendPTSVariable =
    | Data of constructorName:string * constructorArgs:(BackendPTSVariable list)
    | FunctionValue of Term
    | InductiveType of BackendPTSVariable

type CompileExpression =
    /// Erased expressions, typically.
    | EmptyExpr
    /// Integers, chars, strings, etc
    | LiteralExp of Literal
    /// Integer addtion, primitive string functions, etc
    | CompPrimFn of PrimitiveFunction
    | PTSVariable of TypedVar
    | CompApp of CompileExpression * CompileExpression


let rec termToCompileExpr (term:Term) =
    match term with
    | Empty -> EmptyExpr
    | LitConstant l -> LiteralExp l
    | PrimFn f -> CompPrimFn f
    // By the time we've gotten here, Equality
    // has been verified and this is empty.
    | Application(t1,t2) ->
        CompApp(termToCompileExpr t1,termToCompileExpr t2)
  //  | Equality(_,_,_) -> EmptyExpr
