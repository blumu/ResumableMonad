/// Copyright William Blum - October 2015
///
/// Working version of resumable monad
/// Encodes history using F# triples (u,a,v) of type 'u * 'a *'v
module ResumableMonad.PairEncoding

/// A cell will store the result of a previous execution
/// of an expression of type 't.
type Cell<'t> = 
    | NotExecuted
    | Result of 't

/// This is our resumable data type returned by the monadic expressions
/// we are about to define
type Resumable<'h,'t> = 'h -> 'h * Cell<'t>

type ResumableBuilder() =
    member b.Zero() =
        fun () -> (), NotExecuted
    member b.Return(x:'t) =
        fun () -> (), (Result x)
    member b.ReturnFrom(x) =
        x
    member b.Delay(generator:unit->Resumable<'u,'a>) =
        fun h -> generator() h
    
    member b.Bind(
                    f:Resumable<'u,'a>, 
                    g:'a->Resumable<'v, 'b>
           ) 
           : Resumable<'u * Cell<'a> * 'v, 'b> = 
        
        fun (u: 'u, a : Cell<'a>, v : 'v) ->
            match a with
            | NotExecuted -> 
                // The result of f is misssing, we thus
                // advance f's computation by one step
                let u_stepped, a_stepped = f u
                (u_stepped, a_stepped, v), NotExecuted
    
            | Result _a ->
                // Since f's computation has finished
                // we advance g's computation by one step.
                let v_stepped, b_stepped = (g _a) v
                (u, a, v_stepped), b_stepped


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
        // NOTE: do! is not supported yet sow we use let! _ = instead
        let! _ = resumable { printfn "hello2"
                             return () }
        let! x = resumable { return _f1 "a" }
        let w = x + "test"
        let! y = resumable { return _f2 "b" }
        let! z = resumable { return _f2 "c" }
        return x,y, z, w
    }


let execute (r:Resumable<'h,'b>) a = r a
let z<'a,'v> (r:'v) = ((),(NotExecuted:Cell<'a>),r)
let z0<'a> = z<'a,unit> ()

/// Initial state explicitly typed
let s0_explicit = z<unit,_> << z<string,_> << z<int,_> << z<int,_> <| ()
/// Simpler equivalent definition of initial state
let s0_implicit = z << z << z << z <| ()
let s1,_ = execute m s0_implicit
let s2,_ = execute m s1 
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4


/// Another example
let m2 =
    resumable {
        let! x = resumable { return _f1 "a" }
        return! resumable { return _f2 "b" }
    }
