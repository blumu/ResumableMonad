module Example

#load "Zero.fs"
#load "ResumableMonad.Multistep.fs"
open Zero
open ResumableMonad.Multipstep

let x1 = resumable { return 1 }

let x2 = resumable {
    let! y = x1
    return y
}

let example = resumable {
    let! x = resumable { return 1 }
    let! y = resumable { return 2 }
    return x + y
}

let example2 = resumable {
    let! x = example
    let! y = resumable { return 3 }
    return x + y
}

example.resume <| (None, None)
example.resume <| (None, Some 4)

example2.resume <| (None, (None,None), None)
example2.resume <| (Some 3, (None,None), Some 4)

example.resume <| Zero.getZeroTyped<_>

(*** hide ***)
module WhileTest =
    let x = ref 0
    let m =
        resumable {
            printfn "Start"
            while !x<3 do
                printfn "x = %d" !x
                incr x
            printfn "end"
            return !x , true
        }

    let s1 = m.resume (Zero.getZeroTyped<_>)


module WhileTest2 =
    let m2 =
        resumable {
            printfn "Start m2"
            let! z, f = WhileTest.m
            printfn "end m2"
            return z+ 100
        }

    let s1 = m2.resume (Zero.getZeroTyped<_>)
    let s2 = m2.resume (Some (54,true))

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

    let s1 = iftest.resume (Zero.getZeroTyped<_>)


module MyResumableService =
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

    myServiceApi.resume <| myServiceApi.initial


