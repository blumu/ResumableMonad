module MonadicResultPairHistTree

[<StructuredFormatDisplay("[{U},({A}),{V}]")>]
type ITriple<'u,'a,'v> = 
    abstract member U : 'u
    abstract member A : 'a
    abstract member V : 'v

type FakeUnit = FakeUnit

// type X<'t> = 
//     abstract member Zero : unit -> 't

[<StructuredFormatDisplay("ε")>]
type Epsilon() =
    //interface X<FakeUnit> with
    //    member __.Zero () = FakeUnit
    //interface ITriple<FakeUnit,Cell<FakeUnit>,FakeUnit> with
    //    member __.U = FakeUnit
    //    member __.A = NotExecuted
    //    member __.V = FakeUnit
    override __.ToString() = "ε"

and Cell<'t> = 
    | NotExecuted
    | Result of 't

    interface ITriple<Epsilon,Cell<'t>,Epsilon> with
        member __.U = Epsilon()
        member __.A = NotExecuted
        member __.V = Epsilon()

let epsilon = Epsilon()


type Resumable<'h,'t> =
    {
        resume : 'h -> 'h * Cell<'t>
        //zero_previous : 'h
        //zero_last : 't
    }


//[<StructuredFormatDisplay("[{U},{A},{V}]")>]
type Triple<'u,'a,'v>(u:'u,a:Cell<'a>,v:'v) =

    member __.IAmATriple () = ()
    interface ITriple<'u,Cell<'a>,'v> with
        member x.U = u
        member x.A = a
        member x.V = v
    //interface X<'a> with
    //    member __.Zero () = Unchecked.defaultof<'a>
    override x.ToString() =
        sprintf "%A (%A) %O" u a v

// Triple(epsilon,Result 5,epsilon)

type ResumableBuilder() =
    member b.Zero() =
        { 
            resume = fun _ -> epsilon, NotExecuted
            //zero_previous = ()
            //zero_last = ()
        } 
    member b.Return(x:'t) =
        { 
            resume = fun _ -> epsilon, (Result x)
            //zero_previous = ()
            //zero_last = Unchecked.defaultof<'t>
        }
    member b.Delay(generator:unit->Resumable<'u,'a>) =
        { 
            resume = fun x -> generator().resume x
            //zero_previous = generator().zero_previous
            //zero_last = Unchecked.defaultof<'a>
        }
    member b.ReturnFrom(x) = x
    member b.Bind<'u,'a,'v,'b, 'x,'y,'z, 'x2,'y2,'z2
                     when 'u :> ITriple<'x,'y,'z> 
                      and 'v :> ITriple<'x2,'y2,'z2> > 
                (
                    f:Resumable<'u,'a>,
                    g:'a->Resumable<'v, 'b>)
                      :  Resumable<ITriple<'u, Cell<'a>, 'v>, 'b> = 
        { 
            //zero_previous = (g f.zero_last).zero_previous
            //zero_last = (g f.zero_last).zero_last
            resume = fun (X : ITriple<'u, Cell<'a>, 'v>) ->
                         match X.A with
                         | NotExecuted ->
                             // The result of f is misssing, we thus
                             // advance f's computation by one step
                             let u_stepped, a_stepped = f.resume X.U
                             Triple(u_stepped, a_stepped, X.V):>ITriple<_,_,_>, NotExecuted
    
                         | Result _a ->
                             /// f's computation has finished. Advance g's computation by one step
                             let b_resumable = g _a
                             let v_stepped, b_stepped = b_resumable.resume X.V
                             Triple(X.U, X.A, v_stepped):>ITriple<_,_,_>, b_stepped
        }

    member b.Bind<'a,'v,'b, 'x,'y,'z when 'v :> ITriple<'x,'y,'z> > 
                (
                    f:Resumable<Epsilon,'a>,
                    g:'a->Resumable<'v, 'b>)
                      :  Resumable<ITriple<Epsilon, Cell<'a>, 'v>, 'b> = 
        { 
            //zero_previous = (g f.zero_last).zero_previous
            //zero_last = (g f.zero_last).zero_last
            resume = fun (X : ITriple<Epsilon, Cell<'a>, 'v>) ->
                         match X.A with
                         | NotExecuted ->
                             // The result of f is misssing, we thus
                             // advance f's computation by one step
                             let u_stepped, a_stepped = f.resume X.U
                             Triple(u_stepped, a_stepped, X.V):>ITriple<_,_,_>, NotExecuted
    
                         | Result _a ->
                             /// f's computation has finished. Advance g's computation by one step
                             let b_resumable = g _a
                             let v_stepped, b_stepped = b_resumable.resume X.V
                             Triple(X.U, X.A, v_stepped):>ITriple<_,_,_>, b_stepped
        }

    member b.Bind<'u,'a,'b, 'x,'y,'z when 'u:> ITriple<'x,'y,'z> > 
                (
                    f:Resumable<'u,'a>,
                    g:'a->Resumable<Epsilon, 'b>)
                      :  Resumable<ITriple<'u, Cell<'a>, Epsilon>, 'b> = 
        { 
            //zero_previous = (g f.zero_last).zero_previous
            //zero_last = (g f.zero_last).zero_last
            resume = fun (X : ITriple<'u, Cell<'a>, Epsilon>) ->
                         match X.A with
                         | NotExecuted ->
                             // The result of f is misssing, we thus
                             // advance f's computation by one step
                             let u_stepped, a_stepped = f.resume X.U
                             Triple(u_stepped, a_stepped, X.V):>ITriple<_,_,_>, NotExecuted
    
                         | Result _a ->
                             /// f's computation has finished. Advance g's computation by one step
                             let b_resumable = g _a
                             let v_stepped, b_stepped = b_resumable.resume X.V
                             Triple(X.U, X.A, v_stepped):>ITriple<_,_,_>, b_stepped
        }

    member b.Bind (f:Resumable<Epsilon,'a>,  g:'a->Resumable<Epsilon, 'b>)
                      :  Resumable<Cell<'a>, 'b> = 
        { 
            //zero_previous = (g f.zero_last).zero_previous
            //zero_last = (g f.zero_last).zero_last
            resume = fun (a : Cell<'a>) ->
                         match a with
                         | NotExecuted ->
                             // The result of f is misssing, we thus
                             // advance f's computation by one step
                             let u_stepped, a_stepped = f.resume epsilon
                             a_stepped, NotExecuted
    
                         | Result _a ->
                             /// f's computation has finished. Advance g's computation by one step
                             let b_resumable = g _a
                             let _, b_stepped = b_resumable.resume epsilon
                             a, b_stepped
        }

let resumable = new ResumableBuilder()

let _askString label = 
    printfn "[%s] enter new string: " label
    let x3 = System.Console.ReadLine()
    x3

let _randomNumber label =
    let x2 = System.Random().Next()
    printfn "[%s] new number generated: %d" label x2
    x2


/// type Zero<'r> =
///     static member zero<'a, 'r,'x,'y, 'z when 'a :> Triple<'x,'y,'z>> () = Triple((), None, zero<'z>)
/// 
///     member __.zero<unit,r> = Triple((),None,r)

//type Zero<'r> =
//    static member inline zero< ^a when ^a : (static member IAmATriple : ^a-> unit)> () = 
//        Triple((), None, zero<'a>())
//
//type Zero2< 't, ^a when ^t : (static member IAmATripleOf : ^a-> unit) 
//                    and ^a : (static member IAmATriple : unit-> unit) 
//                    > =
//    static member inline zero2() = 
//        Triple((), None, Zero2<'a>.zero2())
//

let m =
    resumable {
        // NOTE: do! is not supported yet sow we use let! _ = instead
        let! _ = resumable { printfn "hello2"
                             return () }
        let! x = resumable { //let! z = resumable {return 6}
                             return _askString "a" }
        let w = x + "test"
        let! y = resumable { return _randomNumber "b" }
        let! z = resumable { return _randomNumber "c" }
        return x, y, z, w
    }

let execute (r:Resumable<'h,'b>) a = r.resume a
let r<'a,'v> (r:'v) = Triple(epsilon,(NotExecuted:Cell<'a>),r) :> ITriple<_,_,_>
let c<'a> () = (NotExecuted:Cell<'a>)

let s2,t2 = execute m (r << r << r << c <| ())
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4
let s6,rr = execute m s5

