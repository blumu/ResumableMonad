module MonadicReplayPairHistTree

type Cell<'t> = 
    | None
    | Replay of 't

type Resumable<'h,'t> =
    {
        resume : 'h -> 'h * Cell<'t>
        zero_previous : 'h
        zero_last : 't
    }

type Triple<'u,'a,'v>(u:'u,a:Cell<'a>,v:'v) =
    class
        member x.U = u
        member x.A = a
        member x.V = v
    end

type ResumableBuilder() =
    member b.Zero()                                        = { resume = function () -> (), None
                                                               zero_previous = ()
                                                               zero_last = () } 
    member b.Return(x:'t)                                  = { resume = function () -> (), (Replay x)
                                                               zero_previous = ()
                                                               zero_last = Unchecked.defaultof<'t>
                                                             }
    member b.Delay(generator:unit->Resumable<'u,'a>)       = { resume = fun x -> let m = generator() in m.resume x
                                                               zero_previous = 
                                                               zero_last = Unchecked.defaultof<'a>
                                                             }
    member b.ReturnFrom(x)                                 = x
    member b.Bind (f:Resumable<'u,'a>,  g:'a->Resumable<'v, 'b>)  
                      :  Resumable<Triple<'u, 'a, 'v>, 'b> = 
        { 
            zero_previous = (g f.zero_last).zero_previous
            zero_last = (g f.zero_last).zero_last
            resume = function (X : Triple<'u, 'a, 'v>) ->
                                match X.A with
                                | None ->
                                    // advance f's computation by one step
                                    let u_stepped, a_stepped = f.resume X.U
                                    Triple(u_stepped, a_stepped, X.V), None
    
                                | Replay _a ->
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


module Resumable =
    let execute (r:Resumable<'h,'b>) a = r.resume a

    let z<'a,'v> (r:'v) = Triple((),(None:Cell<'a>),r)
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
open Resumable

let m =
    resumable {
        printfn "hello"
        // do! resumable { printfn "hello2" } // BUGGY. not supported yet
        let! x = resumable { return _askString "a" }
        let! y = resumable { return _randomNumber "b" }
        return x, y
    }


let s2,t2 = execute m (() |> z |> z)
//let s2,t2 = execute m (z <| z() <| z0)
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4
let s6,r = execute m s5

