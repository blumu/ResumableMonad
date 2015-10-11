﻿module MonadicResultPairHistTree

type Cell<'t> = 
    | NotExecuted
    | Result of 't

type Resumable<'h,'t> =
    {
        resume : 'h -> 'h * Cell<'t>
        //zero_previous : 'h
        //zero_last : 't
    }

type FakeUnit = FakeUnit

type X<'t> = 
    abstract member Zero : unit -> 't

[<StructuredFormatDisplay("ε")>]
type Epsilon() =
    interface X<FakeUnit> with
        member __.Zero () = FakeUnit
    override __.ToString() = "ε"

[<StructuredFormatDisplay("[ {U}, {A}, {V} ]")>]
type Triple<'u,'a,'v>(u:'u,a:Cell<'a>,v:'v) =
    member x.U = u
    member x.A = a
    member x.V = v
    override x.ToString() =
        sprintf "%A %A %O" u a v
    interface X<'a> with
        member __.Zero () = Unchecked.defaultof<'a>

// Triple(Epsilon(),Result 5,Epsilon())

type ResumableBuilder() =
    member b.Zero() =
        { 
            resume = fun () -> (), NotExecuted
            //zero_previous = ()
            //zero_last = ()
        } 
    member b.Return(x:'t) =
        { 
            resume = fun () -> (), (Result x)
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
    member b.Bind (f:Resumable<'u,'a>,  g:'a->Resumable<'v, 'b>)
                      :  Resumable<Triple<'u, 'a, 'v>, 'b> = 
        { 
            //zero_previous = (g f.zero_last).zero_previous
            //zero_last = (g f.zero_last).zero_last
            resume = fun (X : Triple<'u, 'a, 'v>) ->
                         match X.A with
                         | NotExecuted ->
                             // The result of f is misssing, we thus
                             // advance f's computation by one step
                             let u_stepped, a_stepped = f.resume X.U
                             Triple(u_stepped, a_stepped, X.V), NotExecuted
    
                         | Result _a ->
                             /// f's computation has finished. Advance g's computation by one step
                             let b_resumable = g _a
                             let v_stepped, b_stepped = b_resumable.resume X.V
                             Triple(X.U, X.A, v_stepped), b_stepped
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



let execute (r:Resumable<'h,'b>) a = r.resume a
let z<'a,'v> (r:'v) = Triple((),(NotExecuted:Cell<'a>),r)
let z0<'a> = z<'a,unit> ()

    //let rec zz = function 
    //             | 1 -> z () ()
    //             | n -> z () (zz (n-1))

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
        let! x = resumable { return _askString "a" }
        let w = x + "test"
        let! y = resumable { return _randomNumber "b" }
        let! z = resumable { return _randomNumber "c" }
        return x,y, z, w
    }

let s2,t2 = execute m (z << z << z << z <| ())
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4
let s6,r = execute m s5

