(**
Resumable monad, idempotence and online services
================================================

_October 18th, 2015_, **by William Blum**

__Abstract__ We define a new monad and associated F# syntactic sugar ``resumable { ... }``
to express computations that can be interrupted at specified control points. 
Such operation can then be resumed in a subsequent execution 
from the state where it was previously interrupted.

Resumable expresssion makes it simpler to write idempotent code.
Such construct helps simplify how developer write code powering up 
on-line services.

*)


(**
## Motivating example

Suppose that we are building a service that creates virtual machines.
The first step is to obtain the name of the machine to be created.
The second step is to send an asynchronous VM provisioning request to some external cloud provider (e.g., Amazon, Azure, ...).
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
Now let's define a simple model of the IaaS service API used to 
provision new virtual machines. The function below is just a mockup for the real API, it just returns
a random number representing the request ID created on the IaaS side.
*)
    let provisionVM machineName =
        let requestId = System.Random().Next()
        printfn "Provisioning new VM %s in the cloud...." machineName
        printfn "Request ID returned from API is: %d" requestId
        requestId

(**
Last we model the IaaS API that checks the status of a VM deployment.
To simulate the processing time normally required for such operation to complete 
we count how many times the function was calle and after 5 attempts we return 'success'.
*)
    let vmRequestSucceeded =
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
    let myServiceApi () =
        let machineName = Environment.getMachineName ()
    
        let requestId = Environment.provisionVM machineName

        while not <| Environment.vmRequestSucceeded requestId  do
            printfn "Waiting for cloud request to complete..."
            System.Threading.Thread.Sleep(1000)

        printfn "Request completed for machine %s!" machineName
        machineName, requestId

(**
The logic is straightforward: we get the machine name from the client who made the request, we then forward
the request to the cloud and then poll until the cloud request completes.
*)

(**
This works very well except that, typically, service running in the cloud (e.g., as a worker role or a micro service)
can be interrupted at any moment. The machine hosting the service can be restarted, upgarded, or the service rescaled.
In the example above the operation can be interrupted at any point. Suppose for instance that it is stopped
right after sending the VM provisioning request to the cloud. What happens next? Typically 
the infrastructure on which we run our service will detect failure to complete the request after a certain timeout 
and will schedule a new request.
At some point this new request will be picked up by another instance of our service.
When this happens we want the operation to resume where it left off instead of restarting from scratch.
If we don't handle this situation we may end up with an orphan virutal machine and pay for two virtual machines instead of one!

To achived this we need a mechanism to define resumable control points in our implementation. There should be
one resumable point each time an important unit of work is completed. The set of resumable points implicitely defines 
a global progress state for our operation. Such state can be saved somewhere (for instance on a cloud blob or queue) so 
that when our function is called again it can read the state and starts where it left off. 

Such code transformation can be done manually but it's a tedious error-prone task: it basically boils down to converting
your code into a state machine. This requires you to first identify the resumable points in your code,
identify the intermediate states at each resumable points, and implement some 
state serialization/deserialization logic.

What if all this boiler plate code could be generated automatically?
What if we could just implement our service operation with almost identical code as above
and have the state machine logic generated for us?

Here is how we would like to write it:
*)
(*** include:myResuambleService ***)


(**
# Resumable computational expression

We need a data type to encode the state of the computation. What is the state? It's the 
sequence of all results returned at each resumable control point in the computation.

We introduce the `Cell<'t>` type to hold the result of type `'t` returned at one individual resumable control point:
*)
type Cell<'t> = 
| NotExecuted
| Result of 't

(**
There is one cell for each resumable control point in the computation. Before the corresponding operation is executed
the cell contains `NotExecuted`, once it has been executed the cell holds the return 
result yielded at the control point.
 
A resumable control point can then be encoded as a function taking 
the current trace of type `'h` as parameter and returning the new trace 
together with the new value produced at this control point:

*)
type Resumable<'h,'t> = 'h -> 'h * Cell<'t>

(**

Finally here comes the meat dish: the definition of the monadic operators.

*)

type ResumableBuilder() =
    member __.Zero() =
        fun () -> (), (Result())
    
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

The idea in the above definition is to decompose a trace of execution as a triple of the form `(u,a,v)`:

- The `u` part represents a prefix of the trace.
- The `a` part represents the cell in context.
- The `v` part represents the suffix of the trace, that is the result returned at each 
resumable control point following the cell in context.

I've tried several other encodings before I came up with this one.
It may seem more obvious for instance to encode traces with pairs of the form 
`(prefix,last)` where `prefix` is the list of past executed operations and `last` represents the 
last operation to execute. Unfortunately defining monadic operators based on such encodings becomes 
an insurmountable task. The triple decomposition appears necessary to deal with compositionality in the 
implementation of the `Bind` monadic operator.

> A much better encoding would be to define the trace as an heterogenous list
> of elements but the fairly limited type system of F# prevents you from doing that. You
> would have to give up type safety and define your trace as an array of `obj` and appeal to .Net reflection 
> to make this all work. Of course I am not willing to give up type safety so I did not even go there...

*)

(**
We can now define syntactic sugar in F# for the monadic operators above defined: 

*)
let resumable = new ResumableBuilder()


(** 
Let's try first with a simple example: a computation that creates the pair `(1,2)` in two resumable steps:

*)

module Example =
    let example = resumable {
        let! x = resumable { return 1 }
        let! y = resumable { return 2 }
        return x, y
    }

(** 
The type returned by the `resumable` construct is a function taking the current state of the 
trace as a parameter and returns both the new trace and the overall return value.
In order to call this function we need to pass the inital value of the trace as a parameter
(we will get back to this later in more details).

*)
    let s0 = ((), NotExecuted, ((), NotExecuted, ()))
    let s1, r1 = example s0
    let s2, r2 = example s1
    let s3, r3 = example s2

(** 
Each call to `example` advance the computation by one step. The first call produces `1`,
the second call produces `2` while the last `all returns the expect result of `Result(1,2)`.
At each step the current trace state is returned and can be serialized to some external storage.
If the computer runnig the code fails or restart the state can be deserialized and resumed 
where it left off.
*)

(**
## Generating the initial trace state

This works all very well but in practice to actually execute a resumable expression you need
to construct yourself the initial state (or in monadic parlance the zero value of the type).
One thing to notice is that the type of the trace is automatically generated by the monadic syntax.
In the example above it is:

    val example :
          (unit * Cell<int> * (unit * Cell<int> * unit) ->
            (unit * Cell<int> * (unit * Cell<int> * unit)) * Cell<int * int>)

The initial value for this trace type is therefore
*)
    let z0 = ((), NotExecuted, ((), NotExecuted, ()))

(**
The challenge is that this type gets bigger as the number of resuming point increases
in your resumable expression, and so does the F# expression encoding the initial state value.
The more control points you have in your computation the more complex the overall type gets. 

So we need a better way to generate the initial value of the trace type for larger computations.
How can we do that? One way is to define a help function with a phantom type `'v':
*)
    let z<'a,'v> (r:'v) = ((),(NotExecuted:Cell<'a>),r)

(** We can then equivalently define `z0` as:
*)
    let z0_b = z<int,_> << z<int,_> <| ()

(**
We can simplify this further: since the value z0 is passed as a parameter to the resumable computation later in the program 
the type of each cell is automatically inferred therfore we can omit all type parameters:
*)
    let z0_c = z<_,_> << z<_,_> <| ()
    example z0_c

(**
In fact F# let's you just write:
*)
    let z0_d = z << z <| ()
    example z0_d

(** 
More generally, if your computation as `n` resumable control points the initial trace value is defined as above with instead `n` repetitions of the operator `z'.

OK that's pretty neat but you still need to manually count how many resumable points you have in your computation to generate the
initial state. Not only that but the trick actually stops working when you have nested resumable expressions!

## Generating the initial trace value using reflection

I've spent an entire weekend battling with the F# type system to find a type-safe method to define the 
initial state to no avail. It's once again limitations of the F# type system that makes it impossible to define
the terminal element of the trace type in a type-safe manner. The core of the issue boils down to the impossiblity of pattern matching
on types in F#. Instead the solution I came up with involves arcane incantations to the .Net and F# type reflection APIs.


*)
module Zero =
    open Microsoft.FSharp.Reflection

    let rec getZeroUntyped<'X> (_type:System.Type) =
        if _type = typeof<unit> then
            box ()
        elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof<Cell<_>> then
            FSharpValue.MakeUnion (FSharpType.GetUnionCases(_type).[0], [||])
        elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof< _*_*_ > then
            FSharpValue.MakeTuple (_type.GenericTypeArguments |> Array.map getZeroUntyped, _type)
        else
            invalidArg "_type" ("The provided type is not of valid form. The zero element can only be " +
                                "calculated for trace types (those are types used to encode the state " +
                                "of a resumable computation.)")

    let getZeroTyped<'X> =
        typeof<'X>
        |> getZeroUntyped
        |> unbox<'X>
(**
Here are some examples of use:
*)
    module Tests = 
        getZeroTyped<unit>
        getZeroTyped<Cell<string> >
        getZeroUntyped typeof<unit * Cell<string> * unit> 

        let y =typeof<unit * Cell<string> * unit> 
        getZeroUntyped y.GenericTypeArguments.[1]

        getZeroTyped<unit * Cell<string> * (unit * Cell<int> * ((unit * Cell<unit> * unit) * Cell<bool> * unit))>

(**

Now that we have the helpers defined we can define the initial trace for the above example as follows

    let _ = example (getZeroTyped<_>)

The phantom type parameter of `getZeroType` is automatically inferred and the zero value automatically generated.

*)


(**

Now let's get back to our motivating example for provisioning virtual machines in the cloud:
*)

(*** define:myResuambleService ***)
module MyResumableService =

    let myServiceApi =
        resumable {
            let! machineName = resumable { return Environment.getMachineName () }
            let! requestId = resumable { return Environment.provisionVM machineName }

            let! vmready = resumable { 
                while not <| Environment.vmRequestSucceeded requestId do
                    printfn "Waiting for cloud request to complete..."
                    System.Threading.Thread.Sleep(1000)
                return true
            }

            printfn "Request completed for machine %s!" machineName
            return machineName, requestId, vmready
        }

(** 
Does this look similar? The actual implementation is almost identical to the original one. We've just 
sprinkled the resumable keyword in strategic places where we expect the computation to be interrupted.
If you have played with F# asynchronous workflows before this concept shoudl be very familiar.

To execute the operation we can just call each individual step as follows:
*)

    let s1,r1 = myServiceApi (Zero.getZeroTyped<_>)   // Get machine name
    let s2,r2 = myServiceApi s1                       // Submit request and get request id
    let s3,r3 = myServiceApi s2                       // Poll machine until ready
    let s4,r4 = myServiceApi s3                       // Polling completed
    let s5,r5 = myServiceApi s4                       // return result


(**
Each call to the API returns a new state holding the current trace of execution.
Executing each step of the copmutation one at a time is tedious and not very practical. Instead of manually calling
the function to advance to each step we can use the following helper to run the function continuously through completion:
*)
let run m =
    let rec aux (state, result) =
        match result with
        | NotExecuted -> m state
        | Result r -> r

    aux (Zero.getZeroTyped<_>, NotExecuted) 

(** 
Our service API can then be called in a single command: 
*)
run MyService.myServiceApi

(**
## Idempotence

Idempotence means obtaining the same result when executing a given function multiple times. This is a desirable
feature for service as it guarantees that the service always stays in a well-defined state.

The key to achieving idempotence is to define resumable points capturing any effect that the copmutation has on the environment.
This is precisely what we di in our service API example; each resumable point captures an effect on the environment:
1. A machine name was requested from the user
2. A request to provision a VM was issued to an external cloud service
3. The polling has completed and the cloud service has honour the request


With our current implementation of ``run``, however, the call to ``run MyService.myServiceApi`` is not idempotent: calling it twice 
will restart the computation form scratch and ask the user for a new machine name each time. 
The last piece of the puzzle missing to put everything together is *serialization*. Each time we advance the computation we want to 
serialize the current trace of execution and persist it to some external storage (blob, queue,...).

Here is a possible implementation using Json serialization:

*)

let runAndPersistProgress m = 
    let rec aux (state, result) =
        match result with
        | NotExecuted -> m state
        | Result r -> r

    aux (Zero.getZeroTyped<_>, NotExecuted) 



(** 
This is where enter **idempotence**.


# References

There are plenty of references on monads on the internet. You can easily find them with your favourite search engine.
Check for instance "state monad".

# Thanks 

To Thomas Petricek for his marvelous literrate programming package for F# that was used to typeset this HTML page.

*)

// TODO: serialization, interruption


To Tomas Petricek for his marvelous literate programming package for F# that was used to typeset this HTML page.



(*** hide ***)
module WhileTest =
    let x =ref 0
    let m2 =
        resumable {
            printfn "Start"
            while !x<3 do
                printfn "x = %d" !x
                incr x
            printfn "end"
            return !x , true
        }

    let s1,r1 = m2 (Zero.getZeroTyped<_>)
    let s2,r2 = m2 s1 
    let s3,r3 = m2 s2
    let s4,r4 = m2 s3
    let s5,r5 = m2 s4


(*** hide ***)
module IfTest =
    let y = ref 0
    let iftest =
        resumable {
            printfn "Start y = %d" !y
            if !y<2 then
                printfn "passed y = %d" !y
                incr y
            else
                printfn "else"
            printfn "end!"
            return !y , true
        }

    let s1,r1 = iftest (Zero.getZeroTyped<_>)
    let s2,r2 = iftest s1 
    let s3,r3 = iftest s2
    let s4,r4 = iftest s3
    let s5,r5 = iftest s4

(**
# Further improvements


*)