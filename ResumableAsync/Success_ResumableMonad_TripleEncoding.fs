/// Encode history using standard F# triples (u,a,v) of type 'u * 'a *'v
///
/// Failed because: does not properly resume where it left off!!
/// because binding function keeps executing g on every call!
module ResumableMonad.PairEncoding

/// A cell will store the result of a previous execution
/// of an expression of type 't.
type Cell<'t> = 
    | None
    | Replay of 't


/// This is our resumable data type returned by the monadic expressions
/// we are about to define
type Resumable<'h,'t> =
    {
        resume : 'h * Cell<'t> -> 'h * Cell<'t>
    }

(**

Here comes the meat: the definition of the monadic operators.

*)

type ResumableBuilder() =
    member b.Zero() =
        { 
            resume = function (), _ -> (), None
        } 

    member b.Return(x) =
        {
            resume = fun h -> (), (Replay x)
        }

    member b.ReturnFrom(x) = x

    member b.Delay(generator:unit->Resumable<'u,'a>) =
        { 
            resume = fun x -> let m = generator() in m.resume x
        }
    
    member b.Bind(  f:Resumable<'u,'a>, 
                    g:'a->Resumable<'v, 'b>
           ) : Resumable<'u * Cell<'a> * 'v, 'b> = 
        { 
            resume = function 
                ((u: 'u, a : Cell<'a>, v : 'v), b : Cell<'b>) as X ->
                match b with
                | Replay _b ->
                    // Computation finished: state unmodified
                    X 
                            
                | None ->
                    // The result of g is missing. We thus 
                    // need to advance the computation by one step
                    match a with
                    | None -> 
                        // The result of f is misssing, we thus
                        // advance f's computation by one step
                        let u_stepped, a_stepped = f.resume (u, None)
                        (u_stepped, a_stepped, v), None
    
                    | Replay _a ->
                        // Since f's computation has finished
                        // we advance g's computation by one step.
                        let b_resumable = g _a
                        let v_stepped, b_stepped = b_resumable.resume (v, None)
                        (u, a, v_stepped), b_stepped
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
    let execute (r:Resumable<'h,'b>) a = r.resume a

    let z<'a,'v> (r:'v) = ((),(None:Cell<'a>),r)
    let z0<'a> = z<'a,unit> ()

open Resumable

let s2 = execute m (() |> z |> z|> z |> z, None)
let s3 = execute m s2
let s4 = execute m s3
let s5 = execute m s4


let m2 =
    resumable {
        let! x = resumable { return _f1 "a" }
        return! resumable { return _f2 "b" }
    }
