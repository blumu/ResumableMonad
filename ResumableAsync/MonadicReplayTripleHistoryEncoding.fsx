(**
Resumable monad, idempotence and Azure
======================================

This article documents my first successfull attempt at implementing a resumable
monad in F#.

__Goal__ Define a new monadic syntax to express computation that can be interrupted
at certain specified points and be resumed at a later point from the state 
where the computation left off.

__Applications__ Resumable expresssion makes it simpler to write resumable and idempotent code.
This can be tremendously useful to developers writing on-line services such as Azure worker roles or micro services.


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


(**
## Motivating example:

Suppose that we are building a service that creates virtual machines.
The first step is to obtain the name of the machine to be created.
The second step is to send an asynchronous VM provisioning request to some external cloud provider (e.g., Azure).
The external provider returns a request ID used to check status of the
request. The final step is to poll the request ID until the request succeeds
or fails (e.g., virtual machine is provisioned or an error occured).

Let's first define helper functions to model this environment.

First we need a function called by our service to
retrieve the details of the request (such as machine name, type of the machine, ...)
For simplicity here we will assume it's just a machine name.
*)

module Environment =
    let getMachineName () = 
        printfn "Enter name of machine: "
        let machineName = System.Console.ReadLine()
        machineName

(**
Now let's define a simple model of the Azure VM provisioning API.
The function justs acts as a mockup for the real Azure VM API. Here we just return
a random number representing the request ID created on the Azure side.
*)
    let provisionVMOnAzure machineName =
        let requestId = System.Random().Next()
        printfn "Provisioning new VM %s on Azure...." machineName
        printfn "Request ID returned from Azure is: %d" requestId
        requestId

(**
    Last we model the Azure API that checks the status of a VM deployment.
    To simulate the processing time normally required for such operation to complete 
    we count how many times the function was calle and after 5 attempts we return 'success'.
*)
    let azureRequestSucceeded =
        let waitTime = ref 0
        let rec aux requestId = 
            printfn "checking status of request %d" requestId
            waitTime := (!waitTime + 1) % 5
            !waitTime = 0
        aux

(**
Now that our environment is defined we can implement our service operation 
operationally as follows. 
*)

module MyService =
    let myOperation () =
        let machineName = Environment.getMachineName ()
    
        let requestId = Environment.provisionVMOnAzure machineName

        while not <| Environment.azureRequestSucceeded requestId  do
            printfn "Waiting for Azure request to complete..."
            System.Threading.Thread.Sleep(1000)

        printfn "Request completed for machine %s!" machineName
        machineName, requestId

(**
The logic is straightforward: we get the machine name from the client who made the request, we then forward
the request to Azure and then poll until the Azure request completes.
*)

(**
This works very well except that because our service runs as a worker role in Azure it can be interrupted 
at any moment. For instance the machine can be restarted, upgarded, or the service rescaled,...
Consequently the operation above could be interrupted at any point. In particular suppose it is stopped
right after the request to provision a VM is sent to Azure. What happens next? Typically 
the cloud infrastructure our service runs on will notice that the request was not completed and will
repost the request once again.
At some point this new request will be picked up by another instance of our service worker role.
When this happens we want the operation to resume where it left off instead of restarting from scratch.
This is important to prevent having two virtual machines deployed in Azure instead of one!

So what we want is the ability to define resume point in our computation. One each time an important 
unit of work is completed. Those resume point implicitely define a state that needs to be saved somewhere
(for instance on an Azure queue) so that when the function `myOperatrion` is called again it can read the state
and starts where it left off. 

Such code transformation can be done manually but it's a tedious error-prone task: it basically boils down to converting
your code into a state machine. This requires you to identify the resumable points in your code,
identify the intermediate states at each resumable points, and implement some 
state serialization/deserialization logic.

What if all this boiler plate code could be generated automatically?
What if we could just implement our service operation with almost identical code as above
and have the state machine logic generated for us?

Here is how we would like to write it:
*)
(*** include:myResumableOperation ***)


(**
# Resumable computational expression

We first need to define a type to encode the state. The state consists of the trace
of all results returned at each resumable point in the computation.


*)

/// A cell will store the result of a previous execution
/// of an expression of type 't.
type Cell<'t> = 
    | NotExecuted
    | Result of 't


(* 


*)

/// This is our resumable data type returned by the monadic expressions
/// we are about to define
type Resumable<'h,'t> = 'h -> 'h * Cell<'t>

(**

Here comes the meat: the definition of the monadic operators.
*)

type ResumableBuilder() =
    member __.Zero() =
        fun () -> (), NotExecuted
    member __.Return(x:'t) =
        fun () -> (), (Result x)
    member __.ReturnFrom(x) =
        x
    member __.Delay(generator:unit->Resumable<'u,'a>) =
        fun h -> generator() h
    
    member __.Bind(
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
                let v_stepped, b_stepped = g _a v
                (u, a, v_stepped), b_stepped

    member __.Combine(p1:Resumable<'u,unit>,p2:Resumable<'v,'b>) :Resumable<'u*Cell<unit>*'v,'b>=
        __.Bind(p1, (fun () -> p2))

    member __.While(gd, prog:Resumable<unit,unit>) : Resumable<unit,unit> =
        let rec whileA gd prog =
            if not <| gd() then
                __.Zero()
            else 
                fun u -> prog u
   
        whileA gd prog

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

The other important point is the definition of the 

*)

let resumable = new ResumableBuilder()

(**
That's it! We have just defined a new kind of ''resumable'' expressions.
Let's try to make use of it on a case study.

*)

(*** define:myResumableOperation  ***)
let myResumableOperation =
    resumable {
        let! machineName = resumable { return Environment.getMachineName () }
    
        let! requestId = resumable { return Environment.provisionVMOnAzure machineName }

        let! vmready = resumable { 
            printfn "Start polling!"
            while not <| Environment.azureRequestSucceeded requestId do
                printfn "Waiting for Azure request to complete..."
                System.Threading.Thread.Sleep(1000)
            printfn "VM ready!!"
            return true
        }

        printfn "Request completed for machine %s!" machineName
        return machineName, requestId
    }

(** 

*)

let z<'a,'v> (r:'v) = ((),(NotExecuted:Cell<'a>),r)

// let s1,_ = myResumableOperation (z << z << z << z <| ())
// let s2,_ = myResumableOperation s1 
// let s3,_ = myResumableOperation s2
// let s4,_ = myResumableOperation s3
// let s5,_ = myResumableOperation s4

/////////////
/// Terminal element
open Microsoft.FSharp.Reflection
let rec getZeroUntyped<'X> (_type:System.Type) =
    if _type = typeof<unit> then
        box ()
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof<Cell<_>> then
        FSharpValue.MakeUnion (FSharpType.GetUnionCases(_type).[0], [||])
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof< _*_*_ > then
        FSharpValue.MakeTuple (_type.GenericTypeArguments |> Array.map getZeroUntyped, _type)
    else
        invalidArg "_type" "The provided type is not of valid form. The zero element can only be calculated for trace types (those are types used to encode the state of a resumable computation.)"

let getZeroTyped<'X> =
    typeof<'X>
    |> getZeroUntyped
    |> unbox<'X>

getZeroTyped<unit>
getZeroTyped<Cell<string> >
getZeroUntyped typeof<unit * Cell<string> * unit> 

let y =typeof<unit * Cell<string> * unit> 
getZeroUntyped y.GenericTypeArguments.[1]

getZeroTyped<unit * Cell<string> * (unit * Cell<int> * ((unit * Cell<unit> * unit) * Cell<bool> * unit))>
//////////////////

let s1,_ = myResumableOperation (getZeroTyped<_>)
let s2,_ = myResumableOperation s1 
let s3,_ = myResumableOperation s2
let s4,_ = myResumableOperation s3
let s5,_ = myResumableOperation s4
