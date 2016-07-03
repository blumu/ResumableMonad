(**
Resumable monad, idempotence and cloud services
===============================================

_October 18th, 2015_, **by William Blum**

## Abstract

We define a new monad and its associated F# syntactic sugar ``resumable { ... }``
to express computations that can be interrupted at specified control points
and resumed in subsequent executions while carrying along state from 
the previous execution.

Resumable expresssions make it simpler to write idempotent and resumable code which is often
necessary when writing cloud services code.

*)


(**
## Motivating example

Suppose that we are building a service that creates virtual machines.
The first step is to obtain the name of the machine to be created.
The second step is to defer the actual virtual machine provisioning to 
an actual cloud provider like Amazon or Azure through 
an asynchronous API call.
The external provider returns a request ID used to check status of the
request. The final step is to poll the request ID until the request succeeds
or fails (e.g., the virtual machine is provisioned or an error occurred).

Let's first define helper functions to model this environment.
*)
module Environment =

(**
First we need a function called by our service to
retrieve the details of the request (such as machine name, type of the machine, ...)
For simplicity here we will assume it's just a machine name.
*)

    let getMachineName () = 
        printfn "Enter name of machine: "
        let machineName = System.Console.ReadLine()
        machineName

(**
Now let's define a simple model of the cloud service API used to 
provision new virtual machines. The function below is just a mockup for the real API: it returns
a random number representing the request ID created by the cloud VM provider.
*)
    let provisionVM machineName =
        let requestId = System.Random().Next()
        printfn "Provisioning new VM %s in the cloud...." machineName
        printfn "Request ID returned from API is: %d" requestId
        requestId

(**
Last we model the cloud API that checks the status of a previously issued request.
To simulate the processing time normally required for such operation to complete 
we count how many times the function was called and after 5 attempts we return 'success'.
*)
    let vmRequestSucceeded =
        let waitTime = ref 0
        let rec aux requestId = 
            printfn "checking status of request %d (waitTime: %d)" requestId !waitTime
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
can be interrupted at any moment. The machine hosting the service can be restarted, upgraded, or rescaled.
In the example above the operation can be interrupted at any point. Suppose for instance that it is stopped
right after sending the VM provisioning request to the cloud. What happens next? Typically 
the infrastructure on which we run our service will detect failure to complete the request after a certain timeout 
and will schedule a new request.
At some point this new request will be picked up by another instance of our service.
When this happens we want the operation to resume where it left off instead of restarting from scratch.
If we don't handle this situation we may end up with an orphan virtual machine, having to pay for two virtual machines instead of one!

To achieve this we need a mechanism to define resumable control points in our implementation. There should be
one resumable point each time an important unit of work is completed. The set of resumable points implicitly defines 
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
(*** include:myResumableService ***)


(**
# Resumable computational expression

We need a data type to encode the state of the computation. The state will be the 
sequence of all results returned at each resumable control point in the computation.
We thus introduce the type `Cell<'t>` to hold the result of type `'t` returned at one individual resumable control point:
*)
type Cell<'t> = 
| NotExecuted
| Result of 't

(**
There is one cell for each resumable control point in the computation. Before the corresponding operation is executed
the cell contains `NotExecuted`, once it has been executed the cell holds the 
result yielded at the control point.
 
A resumable control point can then be encoded as a function taking 
the current trace of type `'h` as parameter and returning the new trace 
together with the new value produced at this control point:

*)
type Resumable<'h,'t> = 'h -> 'h * Cell<'t>

(**

Finally here comes the meaty part: the definition of the monadic operators.
My monadic skills being rather rusty it took me a weekend to figure 
out the implementation details... The result is deceptively simple:

*)

type ResumableBuilder() =
    member __.Zero() =
        fun () -> (), (Result ())
    
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
the second call produces `2` while the last call returns the expect result of `Result(1,2)`.
At each step the current trace state is returned and can be serialized to some external storage.
If the computer running our code fails or restarts the state can be deserialized and resumed 
where it left off.
*)

(**
## Generating the initial trace state

This works all very well but in practice to actually execute a resumable expression you need
to construct the initial state (or in monadic parlance the zero value of the type) yourself.
One thing to notice is that the type of the trace is automatically generated by the monadic syntax.
In the example above it is:

    val example :
          (unit * Cell<int> * (unit * Cell<int> * unit) ->
            (unit * Cell<int> * (unit * Cell<int> * unit)) * Cell<int * int>)

The initial value for this trace type is therefore
*)
    let z0 = ((), NotExecuted, ((), NotExecuted, ()))

(**
The challenge is that this type gets bigger as the number of resumable points increases
in your resumable expression, and so does the F# expression encoding the initial state value.
The more control points you have in your computation the more complex the overall type gets. 

So we need a better way to generate the initial value of the trace type for larger computations.
How can we do that? One way is to define a helper function with a phantom type `'v`:
*)
    let z<'a,'v> (r:'v) = ((),(NotExecuted:Cell<'a>),r)

(** We can then equivalently define `z0` as:
*)
    let z0_b = z<int,_> << z<int,_> <| ()

(**
We can simplify this further: since the value z0 is passed as a parameter to the resumable computation later in the program 
the type of each cell is automatically inferred therefore we can omit all type parameters:
*)
    let z0_c = z<_,_> << z<_,_> <| ()
    example z0_c

(**
In fact F# let's you just write:
*)
    let z0_d = z << z <| ()
    example z0_d

(** 
More generally, if your computation as `n` resumable control points the initial trace value is defined as above with instead `n` repetitions of the operator `z`.

OK, that's pretty neat but you still need to manually count how many resumable points you have in your expression
to be able to automatically generate the initial trace value. Worse: the trick actually stops working when dealing with nested resumable expressions!

## Generating the initial trace through reflection

I've spent another weekend battling with the F# type system 
looking for a type-safe method to define the initial trace value, to no avail. It's once again limitations of the F# type system that makes it impossible to define
the terminal element of the trace type in a type-safe manner. The core of the issue boils down to the impossibility of pattern matching
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

(*** define:myResumableService ***)
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

(*** include:myResumableService ***)

(** 
Does this look familiar? The actual implementation is almost identical to the original non-resumable one. We've just 
sprinkled the resumable keyword in strategic places where we expect the computation to be interrupted.
If you have played with F# asynchronous workflows before this concept should be very familiar.

To run the operation we can just execute each individual step as follows:
*)

    let s1,r1 = myServiceApi (Zero.getZeroTyped<_>)   // Get machine name
    let s2,r2 = myServiceApi s1                       // Submit request and get request id
    let s3,r3 = myServiceApi s2                       // Poll machine until ready
    let s4,r4 = myServiceApi s3                       // Polling completed
    let s5,r5 = myServiceApi s4                       // return result


(**
Each call returns a new state holding the current trace of execution.
Executing each step of the computation one at a time is tedious and not very practical. 
Instead of manually calling
the function to advance by a single step we can use the following helper to run the entire operation through completion:
*)
let rec runThroughCompletion m (state, result) =
    printfn "state: %A result: %A" state result
    match result with
    | NotExecuted -> runThroughCompletion m (m state)
    | Result r -> r

let run m =
    runThroughCompletion m (Zero.getZeroTyped<_>, NotExecuted) 

(** 
Our service API can then be called in a single command: 
*)
run MyResumableService.myServiceApi

(**
## Idempotence

One important property of ``runThroughCompletion`` is that it is __idempotent__.
This means that executing it once, twice or more times consecutively yields the exact same result.
Mathematically this can be expressed as:

     runThroughCompletion(runThroughCompletion(x)) = runThroughCompletion(x)

for all state x.

This is a desirable property for services which can be interrupted for various reasons
like outages, updates, scaling or migrations.
When such event happens the service typically fails over to a redudant instance of the service that will
attempt to execute the same operation again. Making the operation idempotent guarantees 
that the second execution yields a deterministic outcome regardless of whether the first execution completed or not.

## Cancellability and Resumability

Another desirable property is **cancellability**. Meaning that if the system interrupts 
the operation it should leave itself in a well-defined resumable state. In other words it should be possible
to interrupt execution of ``runThroughCompletion`` without bringing the system to a corrupt state. 

> **Note** Here I want to highlight an important limitation of the resumable monad: it guarantees cancellability 
> at resumable points only. If the execution is  interrupted between those resumable points (for instance after 
> the call to the virtual machine provisioning API but before the call returns with the requestId) then the state 
> of the system will be undefined!


To achieve resumability one needs to identify the possible side-effects that the operation has on the environment.
In our example those side-effects are:

1. A machine name was requested from the user;
2. A request to provision a VM was issued to an external cloud service;
3. The polling has completed and the cloud service has honoured the request.

The resumable monad let's you easily capture those side effects and control points using the ``resumable { ... }`` construct.

## Execution engine

The other thing we need is an execution engine that takes advantage of those control points to cancel and resume the computation.
We can achieve that by adding *persistence* to our implementation of ``run``. Each time we advance the computation to a resumable point we can 
save the current trace of execution and persist it to external storage (blob, queue,...). Next time we run the operation
we read the saved state from disk and continue were we left off.

Here is a possible implementation using Json file serialization. (This codes requires the NewtonSoft Json nuget package.):

*)

#r @"..\packages\Newtonsoft.Json.7.0.1\lib\net45\Newtonsoft.Json.dll"

let runResumable (p:Resumable<'h,'t>) fileName = 
    let rec aux (state:'h, result) =
        System.IO.File.WriteAllText(fileName, Newtonsoft.Json.JsonConvert.SerializeObject(state))
        match result with
        | NotExecuted -> 
            aux (p state)
        | Result r -> (state, result)

    let initialState =
        if System.IO.File.Exists fileName then
            Newtonsoft.Json.JsonConvert.DeserializeObject<'h>(System.IO.File.ReadAllText(fileName))
        else
            Zero.getZeroTyped<_>

    aux (initialState, NotExecuted)


(*** hide ***)
if System.IO.File.Exists @"c:\temp\apiprogress.json" then
    System.IO.File.Delete(@"c:\temp\apiprogress.json")

(**
Now let's excute the service API and simulate a service interruption using an async timeout:
*)
let p = async {
    return runResumable MyResumableService.myServiceApi @"c:\temp\apiprogress.json"
}
let t = Async.RunSynchronously(p, 10)

(**
This first run prompts you to enter the machine name, it then submits the request to the cloud and
starts polling for completion before failing due to the forced timeouts of 10ms.
If you now run the same command once again:
*)
let t2 = Async.RunSynchronously(p, 100)

(** Not only you are not prompted a machine name but the machine name and 
request Id are restored from the previous run 
and the VM provisioning request completes successfully! *)

(**
# Conclusion

I hope you enjoyed this article. For the sake of succinctness I am not going to repeat in the conclusion 
what I already discussed in this article so please refer to the abstract for that :-)

Possible improvements include support for other monadic operators
(``TryFinally``, ``TryWith``, ``Using``) and integration with the ``async`` monad
to enable definition of asynchronous resumable computations.

*)

(** 

# References

There are plenty of references on monads on the internet. You can easily find them with your favourite search engine.
One related topic in the monadic world is the so-called "state monad".
Also I am sure somebody else already came up with something similar to the 
resuamble monad. I did not look in the literature myself, if you have a 
reference please send it to me through github and I'll add the reference here.

# Thanks 

To Tomas Petricek for his marvelous literate programming package for F# that was used to typeset this HTML page.

*)


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
