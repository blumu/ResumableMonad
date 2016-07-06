(**
# The persisted multistep resumable monad

This module implements the persisted & multistep implementation of the
resumable monad.
Multistep means that the where the resumable expression is encoded as 
a mapping from the trace history type 'h to the expression's return type 't
as opposed to a single step semantics (`'h -> 'h * Option<'t>`).
Persisted means that the trace of caching points is persisted to some external storage device 
as the computation progresses. If the computation is interrupted for any external reason (e.g., machine shuts down, process killed, machine upgraded, ...)
then the computation can be resumed from the persisted stated and resume from the last evaluated caching point.
*)

/// Persisted implementation of the multistep resumable monad.
module ResumableMonad.MultipstepPersisted

#if INTERACTIVE
#load "zero.fs"
#load "Scripts\load-references-debug.fsx"
#endif


/// Defines a method to persist a trace history (i.e. caching points) to some external device
type Persist<'h> = 'h -> unit

/// Defines the functions to load/save the caching points to some external storage
type CacheStorage<'h> =
    {
        save : Persist<'h>
        load : unit -> 'h option
    }

/// A resumable computation of type `'t` with caching points history of type `'h`.
/// - `'t` is the returned type of the computation
/// - type `'h` is inferred from the monadic expression and encodes the history of caching points in the
///  resumable expression. The hierachy of caching points is encoded with nested tuples, the leaf elements in the
///  hiearachy are of type `'a option` and represent the caching points themselves.
type Resumable<'h,'t> = Resumable of (Persist<'h> -> 'h -> 't)
with
    /// Resume from a specified history of caching points
    member inline R.resume (persist:Persist<'h>) (h:'h) =
        let (Resumable r) = R
        r persist h

    /// Returns the empty history (i.e. no caching point)
    member inline __.initial =
        Zero.getZeroTyped<'h>

    /// Evaluate the resumable expression starting
    /// from the saved history of cached points if it exists,
    /// or from the empty history otherwise
    member inline R.evaluate (storage:CacheStorage<'h>) =
        let (Resumable resume) = R
        printfn "State type is %O" typeof<'h>
        let state =
            match storage.load() with
            | None ->
                printfn "No cached state in storage: starting from initial state"
                R.initial
            | Some state ->
                printfn "Resuming from existing cached state"
                state

        resume storage.save state

/// A resumable computation of type `'t` with no caching point.
/// This extra type is used as a trick to match
/// on type `'h` at compile-type using .net member overloading
/// (since unfortunatley the static constraint `not ('h :> unit)` cannot be expressed in F#).
//
/// It's not theoretically needed but it helps simplify
/// the type encoding `'h` of caching points by eliminating
/// redundant occurrences of `option unit` within
/// larger resumable expressions.
and Resumable<'t> = Startable of (unit -> 't)
with
    member inline R.resume () =
        let (Startable r) = R
        r ()

/// Return the provided value if specified otherwise evaluate the provided function
/// and persist the result to the external storage device
let inline getOrEvaluate (persist:Persist<'a>) (evaluate: unit -> 'a) = function
    | Some cached ->
        printfn "Reusing cached value: %O" cached
        cached
    | None ->
        printfn "Cache miss: evaluating resumable expression..."
        let r = evaluate()
        printfn "Persisting result to cache: %O" r
        persist r
        r

/// Return a persist function that applies a transformation and then falls back to the specified persist function
let inline (+~) (persist:Persist<'h>) (cons:'a->'h) :Persist<'a> = cons >> persist

/// The syntax builder for the Resumable monadic syntax
type ResumableBuilder() =

    member __.Zero<'t>() : Resumable<_> =
        Startable <| fun () -> ()

    member __.Return(x:'t) =
        Startable <| fun () -> x

    member __.Delay(f: unit -> Resumable<'h,'t>) =
        printfn "===Delay"
        Resumable <| (fun (p:Persist<'h>) h -> f().resume p h)

    member __.Delay(f: unit -> Resumable<'t>) =
        Startable <| fun () -> f().resume ()

    // Resumable<'u,'a> -> ('a->Resumable<'v, 'b>) -> Resumable<'a option * 'u * 'v, 'b>
    member inline __.Bind(f:Resumable<'u,'a>, g:'a->Resumable<'v, 'b>) =
        printfn "===1: 'u:%O 'a: %O 'v: %O 'b: %O" typeof<'u> typeof<'a> typeof<'v> typeof<'b>
        Resumable (fun (p:Persist<'a option * 'u * 'v>) (cached:'a option, u:'u, v:'v) ->
                let persista = p +~ fun a -> Some a, u, v
                let persistu = p +~ fun u -> None, u, v
                let a = cached |> getOrEvaluate persista (fun () -> f.resume persistu u)
                (g a).resume (p +~ fun v -> Some a, u, v) v)
  
    // Resumable<'u,'a> -> ('a->Resumable<'b>) -> Resumable<'a option * 'u, 'b>
    member inline __.Bind(f:Resumable<'u,'a>, g:'a->Resumable<'b>) =
        printfn "===2: 'u: %O 'a: %O 'b:%O" typeof<'u> typeof<'a> typeof<'b>
        Resumable (fun (p:Persist<'a option * 'u>) (cached, u) ->
                        let persista = p +~ fun a -> Some a, u
                        let persistu = p +~ fun u -> None, u
                        (cached |> getOrEvaluate persista (fun () -> f.resume persistu u) |> g).resume())

    // Resumable<'a> -> ('a->Resumable<'v, 'b>) -> Resumable<'a option * 'v, 'b> =
    member inline __.Bind(f:Resumable<'a>, g:'a->Resumable<'v, 'b>) =
        printfn "===3: 'a: %O 'v: %O 'b:%O" typeof<'a> typeof<'v> typeof<'b>
        Resumable (fun (p:Persist<'a option * 'v>) (cached:'a option, v) -> 
                        let a :'a = cached |> getOrEvaluate (p +~ fun a -> Some a, v) f.resume
                        (g a).resume (p +~ fun v -> Some a, v) v)

    // Resumable<'a> -> ('a->Resumable<'b>) -> Resumable<'a option, 'b> =
    member inline __.Bind(f:Resumable<'a>, g:'a->Resumable<'b>) =
        printfn "===4: 'a: %O 'b: %O" typeof<'a> typeof<'b>
        Resumable (fun (p:Persist<'a option>) cached ->
                    let a = cached |> getOrEvaluate (p +~ Some) f.resume
                    (g a).resume())

    // Resumable<'a> -> ('a->Resumable<'b>) -> Resumable<'b>
    member inline __.BindNoCache(f:Resumable<'a>, g:'a->Resumable<'b>) =
        printfn "===5: 'a: %O 'b: %O" typeof<'a> typeof<'b>
        Startable (fun () -> (g <| f.resume()).resume())

    // Resumable<'u,unit> -> Resumable<'v,'b> -> Resumable<'u * 'v,'b>
    member inline __.Combine(p1:Resumable<'u,unit>, p2:Resumable<'v,'b>) =
        printfn "===6"
        Resumable (fun (p:Persist<'u * 'v>) (u, v) ->
                                p1.resume (p +~ fun u -> u, v) u
                                p2.resume (p +~ fun v -> u, v) v)

    // Resumable<unit> -> Resumable<'b> -> Resumable<'b>
    member inline __.Combine(p1:Resumable<unit>, p2:Resumable<'b>) =
        printfn "===7"
        Startable (fun () -> p1.resume(); p2.resume())

    member __.While(condition, body:Resumable<unit>) : Resumable<unit> =
        if condition() then
            __.BindNoCache(body, (fun () -> __.While(condition, body)))
        else
            __.Zero()

(**
We now define the computational expression `resumable { ... }` with all
the syntactic sugar automatically inferred from the above monadic operators.
*)
let resumable = ResumableBuilder()

/// File storage based on Newtonsoft serialization
let inline NewtonsoftStorage< ^T> fileName =
    {
        save = fun history -> System.IO.File.WriteAllText(fileName, Newtonsoft.Json.JsonConvert.SerializeObject(history))
        load = fun () ->
                if System.IO.File.Exists fileName then
                    Some <| Newtonsoft.Json.JsonConvert.DeserializeObject< ^T>(System.IO.File.ReadAllText(fileName))
                else
                    None
    }

/// File storage based on FSharpLu.Json
let inline LuStorage< ^T> fileName =
    {
        save = Microsoft.FSharpLu.Json.Compact.serializeToFile fileName
        load = fun () ->
                    if System.IO.File.Exists fileName then
                        printfn "Loading cached points from file %s" fileName
                        let cache = Microsoft.FSharpLu.Json.Compact.tryDeserializeFile< ^T> fileName
                        match cache with
                        | Choice1Of2 cache -> Some cache
                        | Choice2Of2 _ -> None

                    else
                        printfn "Cache history file not found, starting computation from scratch."
                        None
    }

(**
A simple example: finding three large prime numbers and summing them up
*)
module Example =
    open System

    let isPrime = function
    | 1 -> false
    | 2 -> true
    | n -> { 2..(int <| Math.Ceiling(Math.Sqrt(float n))) } |> Seq.forall (fun d -> n % d <> 0)

    let nthprime n =
        printfn "Calculating %dth prime..." n
        let p = Seq.initInfinite id |> Seq.where isPrime |> Seq.item n
        printfn "%dth prime is: %d" n p
        p

    let addPrimes n1 n2 n3 =
        resumable {
            printfn "Starting computation"

            let! p1 = resumable { return nthprime n1 }

            printfn "Press enter to continue or CTRL+BREAK to pause the calculation here."
            System.Console.Read() |> ignore

            let! p2 = resumable { return nthprime n2 }

            printfn "Press enter to continue or CTRL+BREAK to pause the calculation here."
            System.Console.Read()|> ignore

            let! p3 = resumable { return nthprime n3 }

            let sum = p1 + p2 + p3
            printfn "The sum of %dth prime, %dth prime and %dth prime is %d"  n1 n2 n3 sum
            return sum
        }

    let f = System.IO.Path.GetTempFileName()
    printfn "State file is %s" f

    System.IO.File.Delete(f)
    let result = (addPrimes 30000 20000 10000).evaluate (LuStorage f)
    printfn "Result of the resumable computation is %d" result

    printf "%s" <| System.IO.File.ReadAllText(f)

    
