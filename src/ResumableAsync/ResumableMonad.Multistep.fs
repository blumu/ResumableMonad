(**
Multistep implementation of the Resumable monad
===============================================

This module implements the _multistep_ implementation of the resumable monad
where the resumable expression is encoded as a mapping from the trace
history type 'h to the expression's return type 't.

This contrasts with the implementation from [the original article](TheResumableMonad.fsx) 
where the encoding type `'h -> 'h * Option<'t>`
represents a single state transition. That is a function mapping the existing
trace history to the updated history after taking a single step in the computation.
(i.e. advancing the computation to the next caching point).

The original encoding generates larger types but provides a clean separation
between the definition of the resumable expression monad and the mechanism used
for evaluation and for caching/persistence of the trace history.

The 'mulistep' encoding, defined in the present module, generates smaller types but
requires stronger coupling between the definition of the monadic constructs
and the execution and caching/persistence engine.

*)

/// Multistep resumable monad where a resumable expression is encoded as a 
/// mapping from the trace history type 'h to the expression's return type 't.
module ResumableMonad.Multipstep

/// A resumable computation returning a result of type `'t` with a sequence of
/// caching points encoded by type `'h`.
/// - 'h is a type generated from the monadic expression to encode the history of caching points in the
///  resumable expression. It consists of nested tuples with base elements of type 'a option for each
///  caching point in the computation.
/// - 't is the returned type of the computation
type Resumable<'h,'t> = Resumable of ('h -> 't)
with
    member inline R.resume h =
        let (Resumable r) = R
        r h

    /// Returns the empty history (no caching point are initialized)
    member inline R.initial =
        Zero.getZeroTyped<'h>

(**
The next type we define is not theoretically required to implement the resumable monad,
we introduce it solely to simplify the type encoding of caching points for large resumable expression.
It allows us to eliminate unneeded occurrences of `option unit` 
in the type encoding `'h` of large resumable expressions.

The adverse effect is that we need to define multiple version of the bind monadic operators
for each possible combination of the two `Resumable` type variations: `Resumable<'h, 't>` and `Resumable<'t>`
(If the static constraint `not ('h :> unit)` could be expressed in F# this would not be needed).
*)

/// A resumable computation of type `'t` with no caching point.
and Resumable<'t> = Spawnable of (unit -> 't)
with
    member inline R.resume =
        let (Spawnable r) = R
        r

/// Return the encapsulated value if present otherwise return the result of the specified `evaluate` function
let getOrEvaluate evaluate = function
    | Some cached -> cached
    | None ->
        printfn "Cache miss: evaluating..."
        // This is where caching/persistence needs to be implemented
        evaluate()

/// The syntax builder for the Resumable monadic syntax
type ResumableBuilder() =
    member __.Zero<'t>() : Resumable<_> =
        Spawnable <| fun () -> ()

    member __.Return(x:'t) =
        Spawnable <| fun () -> x

    member __.Delay(f: unit -> Resumable<'h,'t>) =
        Resumable <| fun h -> f().resume h

    member __.Delay(f: unit -> Resumable<'t>) =
        Spawnable <| fun () -> f().resume ()

    // Resumable<'u,'a> -> ('a->Resumable<'v, 'b>) -> Resumable<'a option * 'u * 'v, 'b>
    member inline __.Bind(f:Resumable<'u,'a>, g:'a->Resumable<'v, 'b>) =
        Resumable <| fun (cached, u, v) -> (cached |> getOrEvaluate (fun () -> f.resume u) |> g).resume v

    // Resumable<'u,'a> -> ('a->Resumable<'b>) -> Resumable<'a option * 'u, 'b>
    member inline __.Bind(f:Resumable<'u,'a>, g:'a->Resumable<'b>) =
        Resumable <| fun (cached, u) -> (cached |> getOrEvaluate (fun () -> f.resume u) |> g).resume()

    // Resumable<'a> -> ('a->Resumable<'v, 'b>) -> Resumable<'a option * 'v, 'b> =
    member inline __.Bind(f:Resumable<'a>, g:'a->Resumable<'v, 'b>) =
        Resumable <| fun (cached, v) -> (cached |> getOrEvaluate f.resume |> g).resume v

    // Resumable<'a> -> ('a->Resumable<'b>) -> Resumable<'a option, 'b> =
    member inline __.Bind(f:Resumable<'a>, g:'a->Resumable<'b>) =
        Resumable <| fun cached -> (cached |> getOrEvaluate f.resume |> g).resume()

    // Resumable<'a> -> ('a->Resumable<'b>) -> Resumable<'b>
    member inline __.BindNoCache(f:Resumable<'a>, g:'a->Resumable<'b>) =
        Spawnable <| fun () -> (g <| f.resume()).resume()

    // Resumable<'u,unit> -> Resumable<'v,'b> -> Resumable<'u * 'v,'b>
    member inline __.Combine(p1:Resumable<'u,unit>, p2:Resumable<'v,'b>) =
        Resumable <| fun (u, v) -> p1.resume u; p2.resume v

    // Resumable<unit> -> Resumable<'b> -> Resumable<'b>
    member inline __.Combine(p1:Resumable<unit>, p2:Resumable<'b>) =
        Spawnable <| fun () -> p1.resume(); p2.resume()

    member __.While(condition, body:Resumable<unit>) : Resumable<unit> =
        if condition() then
            __.BindNoCache(body, (fun () -> __.While(condition, body)))
        else
            __.Zero()

(**
We can now declare the computational expression `resumable { ... }` definining
all the syntactic sugar for the monadic operators defined in `ResumableBuilder`.
*)
let resumable = new ResumableBuilder()


(**
## Examples
For usage examples of the resumable monad see the [Examples page](examples.html)

## Acknowledgement

I shared my original article on the resumable monad with Tomas Petricek who
gave intersting feedback and in particular clarified the connection with the reader monad in an 
[F# snippet](http://fssnip.net/tu).
The enconding used in the mulitsep resumable monad is the same as the one used in Tomas's snippet 
however the caching here is performed within the definition of the bind operator as opposed to an external function. 

*)