/// From Okaski, Purely Functional Data Structures
module FStream

module LazyUtils =
    let inline lazyBind (l1:Lazy<'a>) (l2:('a -> Lazy<'b>)) : Lazy<'b> =
        Lazy.Create(fun () -> (l2 (l1.Force())).Force())

    let inline lazyPure a = Lazy.CreateFromValue(a)

    let inline lazyZero< ^a when ^a : (static member Zero : ^a)> () : Lazy< ^a> =
        Lazy.CreateFromValue(LanguagePrimitives.GenericZero)
         
    let inline lazyOne< ^a when ^a : (static member One : ^a)> () : Lazy< ^a> =
        Lazy.CreateFromValue(LanguagePrimitives.GenericOne)

    let inline lazyAdd (a : Lazy< ^a> ) (b: Lazy< ^a> ) : Lazy< ^a> 
            when ^a : (static member (+) : ( ^a * ^a) -> ^a ) =
        Lazy.Create(fun () -> (a.Force()) + (b.Force()))


    type LazyBuilder() = 

        member __.Bind(a,f) = lazyBind a f
        member __.Return(a) = lazyPure a
        member __.ReturnFrom(a) = a


let lazyBuilder = LazyUtils.LazyBuilder()

type StreamCell<'a> = 
    | Nil
    | Cons of 'a * (FStream<'a>)

and FStream<'a> = Lazy<StreamCell<'a>>

[<RequireQualifiedAccess>]
module FStream =

    let rec append (s1:FStream<'a>) (s2:FStream<'a>) : FStream<'a> =
        lazyBuilder {
            match! s1 with
            | Nil -> return! s2
            | Cons(a,s) -> return Cons(a,append s s2)
        }