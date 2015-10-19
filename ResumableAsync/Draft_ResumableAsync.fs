type State<'T> =
    {
        save : 'T -> unit
        read : unit -> 'T
    }

type ResumableAsyncParameters<'S, 'T> =
    {
        /// called when pausing the async to save the current state 'T
        saveState : 'S -> unit

        /// read the current state
        readState : unit -> 'S

        /// underlying async worfklow to execute when resumed
        workflow : Async<'T>
    }



type RecordedResults<'T, 'P> =
    ('T * RecordedResults<'P>) option

// ResumableAsync<'S, 'T> correspond to a resumable async worfklow of type 'T
// with previously captured state 'S
type ResumableAsync<'S, 'T> = 
    | P of ResumableAsyncParameters<'S, 'T>


module ResumableAsyncBuilderImpl =
    let zero =
        P {
            saveState = fun x -> ()
            readState = fun () -> ()
            workflow = async.Zero()
        }
    
    let bind p f =
        p:saveState
        async {
            return! f p
        }
        |> P
        //P <| async.Bind(p1,p2)

open ResumableAsyncBuilderImpl
[<Sealed>]
type ResumableAsyncBuilder<'T>(s:State<'T>) =
    member b.Zero()                 = zero
    member b.Delay(f)               = P <| async.Delay(f)
    //member b.Return(x)              = P <| async.Return(x)
    member b.ReturnFrom(x:Async<_>) = x
    member b.Bind(p1, p2)           = bind
    //member b.Using(g, p)            = P <| async.Using(g,p)
    //member b.While(gd, prog)        = P <| async.While(gd,prog)
    //member b.For(e, prog)           = P <| async.For(e, prog)
    //member b.Combine(p1, p2)        = P <| async.Combine(p1, p2)
    //member b.TryFinally(p, cf)      = P <| async.TryFinally(p, cf)
    //member b.TryWith(p, cf)         = P <| async.TryWith(p, cf)

async.de

let serialize : State<int> = 
    let x = ref 0
    {
        save = fun t -> x := 0
        read = fun () -> !x
    }

let resumable = new ResumableAsyncBuilder<int>(serialize)

let makeResumable (x:Async<'T>) : ResumableAsync<'T> =
        resumable.Return<'T> (x)
    
let resumeAsAsync  (x:ResumableAsync<'t>) : (Async<'T>) =
    async {
        
    }


let provisionvm machineName = async {
        printfn "Spawn a VM computation..."
        let r = System.Random()
        let requestId = r.Next()
        return requestId
    }

let waitForRequestToComplete = async {
        printfn "waiting for operation to complete"
        do! Async.Sleep(10)
    }

let myResumableWorfklow machineName =
    resumable {
        
        let x = 5

        let! request = provisionvm machineName |> makeResumable

        do! waitforRequestToComplete request |> makeResumable

        printfn "VM provisioning completed"

        do! deleteVM |> makeResumable

        printfn "VM deletion completed"

    }

let x = resumeAsAsync <| myResumableWorfklow "machine"
let y = resumeAsAsync <| myResumableWorfklow "machine"
