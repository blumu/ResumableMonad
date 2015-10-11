/// Encode history using functions
module ResumableMonad.FuncEncoding

// history consisting of a single element of type 'a
type H1<'a> = unit -> 'a
// Cons: history consisting of element 'a followed by rest of history of type 'past
type H2<'a, 'past> = unit -> 'a * 'past

// unit -> 'a
// unit -> 'a * (unit -> 'b )
// unit -> 'a * (unit -> 'b * (unit -> 'c))

// So the history "Hello" , 3, "Bla" is encoded as
let sampleHistory : H2<string, H2<int, H1<string>>> =
    fun () -> "Hello", fun () -> 3, fun () -> "Bla"

type Resumable<'u,'t> =
    {
        //resume : H<'t * 'h> -> H<'t * 'h> * 't
        resume : H2<'t option, 'u> -> H2<'t option, 'u>
        //zero : 'h * H<'t>
    }


type ResumableBuilder() =
    member b.Zero()                 = { resume = function (), _ -> (), None
                                        //zero = (), None
                                         } 
    member b.Return(x)              = { resume = fun h -> (), (Replay x)
                                        //zero = (), None 
                                        }
    member b.ReturnFrom(x)          = x

    member b.Bind(f:Resumable<'u,'a>, 
                  g:'a->Resumable<unit, 'b>
                  ) : Resumable<'u * H<'a>, 'b> = 
        { 
        zero = f.zero, None
        resume = function ((u: 'u, a : H<'a>), b : H<'b>) as X ->
                            match b with
                            | Replay _b ->
                                X // Computation finished: state unmodified
                            
                            | None -> // g's result is missing. need to advance computation by one step
                                match a with
                                | None -> 
                                    // advance f's computation by one step
                                    let u_stepped, a_stepped = f.resume (u, a)
                                    (u_stepped, a_stepped), None
    
                                | Replay _a ->
                                    /// f's computation has finished. Advance g's computation by one step
                                    let b_resumable = g _a
                                    let _, b_stepped = b_resumable.resume ((), None)
                                    (u, a), b_stepped
        }

    member b.Bind(f:Resumable<'u,'a>, 
                  g:'a->Resumable<'vq * 'vh, 'b>
                  ) : Resumable<'u * H<'a> * ('vq * 'vh), 'b> = 
        { 
        zero = f.zero,Unchecked.defaultof<'v>), None
        resume = function ((u: 'u, a : H<'a>, v : 'v), b : H<'b>) as X ->
                            match b with
                            | Replay _b ->
                                X // Computation finished: state unmodified
                            
                            | None -> // g's result is missing. need to advance computation by one step
                                match a with
                                | None -> 
                                    // advance f's computation by one step
                                    let u_stepped, a_stepped = f.resume (u, a)
                                    (u_stepped, a_stepped, v), None
    
                                | Replay _a ->
                                    /// f's computation has finished. Advance g's computation by one step
                                    let b_resumable = g _a
                                    let v_stepped, b_stepped = b_resumable.resume (v, None)
                                    (u, a, v_stepped), b_stepped
        }

    //member b.Bind(f:Resumable<'u,'a>, 
    //              g:'a->Resumable<'v, 'b>
    //              ) : Resumable<'u * H<'a> * 'v, 'b> = 
    //    { 
    //    zero = let x,y = f.zero in
    //           (x,y,Unchecked.defaultof<'v>), None
    //    resume = function ((u: 'u, a : H<'a>, v : 'v), b : H<'b>) as X ->
    //                        match b with
    //                        | Replay _b ->
    //                            X // Computation finished: state unmodified
    //                        
    //                        | None -> // g's result is missing. need to advance computation by one step
    //                            match a with
    //                            | None -> 
    //                                // advance f's computation by one step
    //                                let u_stepped, a_stepped = f.resume (u, a)
    //                                (u_stepped, a_stepped, v), None
    //
    //                            | Replay _a ->
    //                                /// f's computation has finished. Advance g's computation by one step
    //                                let b_resumable = g _a
    //                                let v_stepped, b_stepped = b_resumable.resume (v, None)
    //                                (u, a, v_stepped), b_stepped
    //    }


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