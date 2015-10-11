/// Encode history using standard F# triples (u,a,v) of type 'u * 'a *'v
///
/// Failed because: does not properly resume where it left off!!
/// because binding function keeps executing g on every call!
module ResumableMonad.PairEncodingWithZero

/// A cell will store the result of a previous execution
/// of an expression of type 't.
type Cell<'t> = 
    | NotExecuted
    | ResultAndZero of 't * 't

type ResumableState<'h,'t> =
    {
        history : 'h * Cell<'t>
        //zero : 'h * Zero<'t>
    }

/// This is our resumable data type returned by the monadic expressions
/// we are about to define
type Resumable<'h,'t,'n> = 
    {
        resume : 'h -> ResumableState<'h,'t>
        zero_history : 'h * Cell<'t> * 'n
        //zero_cell : 'X -> 't
    }

(**

Here comes the meat: the definition of the monadic operators.

*)

type ResumableBuilder() =
    member b.Zero() =
        { resume = fun () ->
            { 
                history = (), NotExecuted
            } 
          zero_history = (), NotExecuted, () // //, Some ()
          //zero_cell = ()
        }

    member b.Return(x:'t) =
        let z = Unchecked.defaultof<'t>
        { resume = 
            fun h ->
                {
                    history = (), (ResultAndZero(x, z))
                }
          zero_history= (), NotExecuted, ()
          //zero_cell = z
        }

    member b.ReturnFrom(x) = x

    member b.Delay(generator:unit->Resumable<'u,'a,'n>) =
        {  resume = fun h -> generator().resume h
           zero_history = generator().zero_history
           //zero_cell = generator().zero_cell
        }
    
    member b.Bind(  f:Resumable<'u,'a, 'vbn>, 
                    g:'a->Resumable<'v, 'b, 'n>
           ) : Resumable<'u * Cell<'a> * 'v, 'b, 'n> = 
        //let z = Unchecked.defaultof<'b>
        let u,a,vbn = f.zero_history
        { 
            //zero_cell = z
            zero_history = f.zero_history, NotExecuted, v

            resume = function 
                (u: 'u, a : Cell<'a>, v : 'v) ->
                    match a with
                    | NotExecuted -> 
                        // The result of f is misssing, we thus
                        // advance f's computation by one step
                        let r = f.resume u
                        {
                            history =
                                let u_stepped, a_stepped = r.history
                                (u_stepped, a_stepped, v), NotExecuted
                            
                            //zero = 
                            //    let uzero_stepped, azero_stepped = r.zero
                            //    (uzero_stepped, azero_stepped, Unc), None
                        }
    
                    | ResultAndZero (_a, _a_zero) ->
                        // Since f's computation has finished
                        // we advance g's computation by one step.
                        let b_resumable = g _a
                        let r = b_resumable.resume v
                        let v_stepped, b_stepped = r.history
                        {
                            history = (u, a, v_stepped), b_stepped
                            //zero = match b_stepped with
                            //       | None -> None
                            //       | Some v -> r.zero
                        }
                        
        }


let resumable = new ResumableBuilder()

let _f1 label = 
    printfn "[%s] enter new string: " label
    let x3 = System.Console.ReadLine()
    x3

let _f2 label =
    let x2 = System.Random().Next()
    printfn "[%s] new number generated: %d" label x2
    x2

let m =
    resumable {
        let! _ = resumable { printfn "hello2"
                             return () } // BUGGY. not supported yet
        let! x = resumable { return _f1 "a" }
        let w = x + "test"
        let! y = resumable { return _f2 "b" }
        let! z = resumable { return _f2 "c" }
        return x,y, z, w
    }


module Resumable =
    let execute (r:Resumable<'h,'b,_>) a = (r.resume a).history
    let z<'a,'v> (r:'v) = ((),(NotExecuted:Cell<'a>),r)
    let z0<'a> = z<'a,unit> ()

open Resumable
let s0_explicit = z<unit,_> << z<string,_> << z<int,_> << z<int,_> <| ()
m.zero_history ()
let s0 = z << z << z << z <| ()
let s1,_ = execute m s0
let s2,_ = execute m s1 
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4



let m2 =
    resumable {
        let! x = resumable { return _f1 "a" }
        return! resumable { return _f2 "b" }
    }
