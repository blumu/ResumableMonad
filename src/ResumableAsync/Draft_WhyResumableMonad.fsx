
/// One way to introduce motivation for the resumable monad

/////////
/// We start with a non resumable defintion of the function
let nonResumable_f () =
    printfn "enter new string: "
    let x3 = System.Console.ReadLine()
    // want to pause/resume here
    let x2 = System.Random().Next()
    printfn "new number generated: %d" x2
    // want to pause/resume here
    x2,x3

nonResumable_f ()

/// It is not idempotent: calling it twice will result in two separate user prompts for string input
/// and a different random number will be generated each time

/// How to make this code idempotent? First we need to a way to record the history of results
/// of each previously executed operations at each resumable point

/// Let's define a data type to record a history of results (you may recognize the option type if you are familiar with ML programming)
type H<'a> = 
    | None
    | Replay of 'a

/// The history of past results for our function will be encoded by the type:
type History_of_F = H<string * H<int * H<unit>>>

// Here is an empty history
let _h1 = None : History_of_F
/// Here is a history after user entered the string "foo"
let _h2 = Replay ("foo", None) : History_of_F
/// user entered foo and then the numbr 5 was randomly generated
let _h3 = Replay ("d", Replay(5, None)) : History_of_F

/// You would then write the resumable function as:
let resumable_f history =
    let x3, h2 =
        match history with
        | None ->
            printfn "enter new string: "
            let x3 = System.Console.ReadLine()
            x3, None
        | Replay (x3, h2) ->
            printfn "[replayed] %s" x3
            x3, h2

    let x2, h1 =
        match h2 with
        | None ->
            let x2 = System.Random().Next()
            printfn "new number generated: %d" x2
            x2, None
        | Replay (x2, h1) ->
            printfn "[replayed] %d" x2
            x2, h1
    x3,x2

resumable_f <| Replay ("foo", Replay (5, None))
resumable_f <| Replay ("foo", None)
resumable_f <| None


// Works well but code is fairly verbose (each resume point expands to a large match close) and 
// the orginal workflow gets obscured amongst boilterplate code.
// Let's clean it up by factoring common code

let _f1() = 
    printfn "enter new string: "
    let x3 = System.Console.ReadLine()
    x3, None

let _f2() =
    let x2 = System.Random().Next()
    printfn "new number generated: %d" x2
    x2, None

let _f3 x3 x2 = x3,x2

let resumable_f_2 history =
    let x3, h2 =
        match history with
        | None -> _f1()
        | Replay (x3, h2) ->
            printfn "[replayed] %s" x3
            x3, h2

    let x2, h1 =
        match h2 with
        | None -> _f2()
        | Replay (x2, h1) ->
            printfn "[replayed] %d" x2
            x2, h1
    _f3 x3 x2

// Now let's drop the logging and factor out the match close
let m h f =
    match h with
    | None -> f()
    | Replay (x, h) -> x, h

let resumable_f_3 history =
    let x3, h2 = m history _f1
    let x2, h1 = m h2 _f2
    _f3 x3 x2

// Let's rewrite it in equivalent monadic form.

//let bind x f g =

// what we want is both a writer and state monad

// type H2<'a, 'q> = 
//     | Missing
//     | Leaf of 'a
//     | ReplaySeq of H2<'a> * H2<'b>
