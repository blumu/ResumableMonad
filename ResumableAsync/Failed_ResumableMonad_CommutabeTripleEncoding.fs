/// Encode history using commutable triples
module ResumableMonad.CommutableTiples

type Step<'a> =
    | Paused
    | Returned of 'a 

/// Reversed history as commutable triples
type Trace<'u,'a,'v> = 
    // no more history
    | Empty
    
    // one step recorded in the trace returning an element of type 'a
    | Prepend of Step<'a> * 'v

    // concatenation of two traces
    | Append of 'u * Step<'a>

    // // execution was paused, trace stops here, element 'a is missing
    // | Paused of 'u

//module Trace =
//    let tail t =
//        function
//        | Empty -> invalidOp "Trace is empty"
//        | Step a -> a
//        | Append (u, a) -> tail a
         

type Resumable<'u,'a, 'v> =
    {
        //resume : H<'t * 'h> -> H<'t * 'h> * 't
        resume : Trace<'u,'a,'v> -> Trace<'u,'a,'v>
        //zero : 'h * H<'t>
    }


type ResumableBuilder() =
    member b.Zero()                 = { resume = function _ -> Empty
                                        //zero = (), None 
                                        } 
    member b.Return(x)              = { resume = function h -> Prepend(Returned x, Empty)
                                        //zero = (), None
                                       }
    member b.ReturnFrom(x)          = x

    member b.Bind(f:Resumable<'u,'a, unit>, 
                  g:'a->Resumable<unit, 'v, 'b>
                  ) : Resumable<Trace<'u,'a,'v>,'b, unit> =  // 'u * H<'a> * 'v
        { 
        //zero = let x,y = f.zero in                (x,y,Unchecked.defaultof<'v>), None
        resume = function //((u: 'u, a : H<'a>, v : 'v), b : H<'b>) as X ->
                          (X : Trace<Trace<'u,'a,'v>,'b, unit>)->
                            match X with
                            // Step was executed and recorded
                            | Empty -> X
                            | Append (_,b)
                            | Step b ->
                                X // replay same step, state unmodified
    
                            // execution was paused here
                            | Paused b_prefix  ->
                                // g's result is missing. need to advance computation by one step
                                match b_prefix with
                                | Empty -> invalidOp "Bug! can't be empty"
                                | Append (u, _a_v) ->
                                    match _a_v with
                                    | Empty -> invalidOp "Bug! can't be empty"
                                    | Append(a,v) ->
                                        // advance f's computation by one step
                                        let _ua_executed = f.resume (Append(u, a))

                                        let commute (t:Trace<'u,'a>) (v:'v) : Trace<'u, Trace<'a,'v>> =
                                            null

                                        let reorg (u:'u) (a:'a) (t:Trace<'v,'b>) : Trace<Trace<'u,Trace<'a,'v>>,'b> =
                                            null

                                        let b_resumable = g a
                                        reorg u a b_resumable.resume //(Paused b_prefix)

                                        //Append(commute _ua_executed v, b)
                                
                                | Paused _u_a ->

                                // u  and v are empty
                                | Step _a ->
                                    // /// f's computation has finished. Advance g's computation by one step
                                    // let b_resumable = g _a
                                    // let v_stepped, b_stepped = b_resumable.resume (v, None)
                                    // (u, a, v_stepped), b_stepped
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
        let! x = resumable { return _f1 "a" }
        let! y = resumable { return _f2 "b" }
        return y
    }

let m2 =
    resumable {
        let! x = resumable { return _f1 "a" }
        return! resumable { return _f2 "b" }
    }


let execute (r:Resumable<'h,'b>) a = r.resume a
m.resume
let s2 = execute m (((),None,((),None,())), None) //m.zero
let s2 = execute m m.zero
let s3 = execute m s2
let s4 = execute m s3
m.zero
(((),None,()), None)
m2.zero
(((),None,()), None)

let s2 = execute m2 m2.zero // (((),None,()), None)
let s3 = execute m2 s2
let s4 = execute m2 s3


let resumable_f_monadic =
    resumable {
        let x3, h2 = m history f1
        let x2, h1 = m h2 f2
        f3 x3 x2
        ()
    }