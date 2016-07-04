module ResumableMonad

type Resumable<'h,'t> = 'h -> 'h * Option<'t>

type ResumableBuilder() =
    member __.Zero() =
        fun () -> (), (Some ())
    
    member __.Return(x:'t) =
        fun () -> (), (Some x)
    
    member __.ReturnFrom(x) =
        x
    
    member __.Delay(generator:unit->Resumable<'u,'a>) =
        fun h -> generator() h
    
    member __.Bind(
                  f:Resumable<'u,'a>, 
                  g:'a->Resumable<'v, 'b>
           ) 
           : Resumable<'u * Option<'a> * 'v, 'b> = 
        
        fun (u: 'u, a : Option<'a>, v : 'v) ->
            match a with
            | None -> 
                // The result of f is misssing, we thus
                // advance f's computation by one step
                let u_stepped, a_stepped = f u
                (u_stepped, a_stepped, v), None
    
            | Some _a ->
                // Since f's computation has finished
                // we advance g's computation by one step.
                let v_stepped, b_stepped = g _a v
                (u, a, v_stepped), b_stepped

    member __.Combine(p1:Resumable<'u,unit>,p2:Resumable<'v,'b>) :Resumable<'u*Option<unit>*'v,'b>=
        __.Bind(p1, (fun () -> p2))

    member __.While(gd, prog:Resumable<unit,unit>) : Resumable<unit,unit> =
        let rec whileA gd prog =
            if not <| gd() then
                __.Zero()
            else 
                fun u -> prog u
   
        whileA gd prog

(**

The idea in the above definition is to decompose a trace of execution as a triple of the form `(u,a,v)`:

- The `u` part represents a prefix of the trace.
- The `a` part represents the cell in context.
- The `v` part represents the suffix of the trace, that is the result returned at each 
resumable control point following the cell in context.

I've tried several other encodings before I came up with this one.
It may seem more obvious for instance to encode traces with pairs of the form 
`(prefix,last)` where `prefix` is the list of past executed operations and `last` represents the 
last operation to execute. Unfortunately defining monadic operators based on such encodings becomes 
an insurmountable task. The triple decomposition turns out to be necessary to deal with compositionality in the 
implementation of the `Bind` monadic operator.

> A much better encoding would be to define the trace as a heterogeneous list
> of elements but the fairly limited type system provided by F# (compared to Haskell for instance) prevents you from doing that. You
> would have to give up type safety and define your trace as an array of `obj` and appeal to .Net reflection 
> to make this all work. Of course I am not willing to give up type safety so I did not even go there...

*)

(**
We can now define syntactic sugar in F# for the monadic operators above defined: 

*)
let resumable = new ResumableBuilder()


(** 
Let's make use of it on a simple example first: a computation that creates the pair `(1,2)` in two resumable steps:

*)

module Example =
    let example = resumable {
        let! x = resumable { return 1 }
        let! y = resumable { return 2 }
        return x, y
    }