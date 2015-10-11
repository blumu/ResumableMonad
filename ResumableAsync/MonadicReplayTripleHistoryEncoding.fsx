module MonadicReplayPairHist

(**
Resumable monad
===============

This article documents my first successfull attempt at implementing a resumable
monad in F#.

__Drawback__ The type used to encode the
state is automatically inferred by the monadic
expression. Because of the limitation of the F# type system there is no automatic
way to generate the terminal element of this type. Using Unchecked.defaultof would 
just return null instead of the expected value.
This means that to actually run the expression you need
to construct the initial state (or in monadic parlance the zero value of the type) yourself.
The challenge is that this type gets bigger as the number of resuming point increases
in your resumable expression, and so does the F# expression encoding
the initial state value.

*)

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

(**
The main trick in the above definition is the triple decomposition of the form `(u,a,v)`
to represent the history of resumable steps.

- The `u` part represents a prefix of the history.
- The `a` part represents a cell, that is a
result of a single resumble step. If the corresponding operation was already executed
then this holds the result of that expression, otherwise it's just None.
- The `v` part represents the suffix of the history, that is the result of every    
resumable operation executed after the operation that returns 'a'.

I've tried several other encodings before I came up with this one.
It may seem simpler to instead use a pair-based encoding of the form 
(prefix,last) where `prefix` is the list of past executed operations and `last` represents the 
last operation to execute. Unfortunately it did not seem possible 
to define _composable_ monadic operators with such encodings. There are also alternative encodings based on the `obj' type and the use of .Net reflexion
if you are ready to give up on strong typing (which I am not).

The other important point is the definition of the "
*)

let resumable = new ResumableBuilder()


(**
That's it! We have just defined a new kind of ''resumable'' expressions.
Let's try to make use of it on a case study.

## Case Study

Suppose that we are building a service that creates virtual machines.
The first step is to obtain the name of the machine to be created.
The second step is to send an asynchronous request to some cloud provider (e.g., Azure)
to provision a VM, this returns a request ID that can be used to check status of the
request. The final step is to poll the request ID until the request succeeds
or fails (virtual machine is provisioned or an error occured in the service).
 
Let's define helper functions to simulate the environment 
with which our service is interacting.

First we need a function called by our service to
retrieve the details of the request (such as machine name, type of the machine, ...)
For simplicity here we will assume it's just a machine name.
*)

let getMachineName () = 
    printfn "Enter name of machine: "
    let machineName = System.Console.ReadLine()
    machineName

(**
Now let's simulate the Azure VM provisioning API.
This is just implemented as a mockup returning a random number representing the 
 Azure request ID.
*)
let provisionVMOnAzure machineName =
    let requestId = System.Random().Next()
    printfn "Provisioning new VM %s on Azure...." machineName
    printfn "Request ID returned from Azure is: %d" requestId
    requestId

(**
    Now let's write a mockup for the Azure API that check the status of a VM deployment.
    To simulate the long time taken for such operation we just use a variable counting 
    how many time the fnuction was called, after 5 attempts we return success.
*)
let azureRequestSucceeded =
    let waitTime = ref 0
    let rec aux requestId = 
        printfn "checking status of request %d" requestId
        waitTime := (!waitTime + 1) % 5
        !waitTime = 0
    aux

(**
Our service operation could then be implemented operationally as follows
*)
let myService () =
    let machineName = getMachineName ()
    
    let requestId = provisionVMOnAzure machineName

    while not <| azureRequestSucceeded requestId  do
        printfn "Waiting for Azure request to complete..."
        System.Threading.Sleep(100)

    printfn "Request completed for machine %s!" machineName
    return machineName, requestId

(**
The challenge is that our service runs as a worker role that can be interrupted at any moment.
In particular it could be interrupted right after the request to provision a VM was sent to Azure.
When a new instance of the service resumes it needs to resume where it left off instead of just 
naively restarting from scratch. Otherwise you end up having two virtual machines deployed in Azure.
*)

(** 
Now our service can be defined as follows
*)
let service =
    resumable {
        do! resumable { return () }
        let! x = resumable { return _f1 "a" }
        let! y = resumable { return _f2 "b" }
        return y
    }

let execute (r:Resumable<'h,'b>) a = r.resume a
let s2 = execute m (((),None,((),None,((),None,()))), None)
let s3 = execute m s2
let s4 = execute m s3


let m2 =
    resumable {
        let! x = resumable { return _f1 "a" }
        return! resumable { return _f2 "b" }
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