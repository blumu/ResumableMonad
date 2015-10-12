module MonadicResultPairHistTree

[<StructuredFormatDisplay("ε")>]
type Epsilon() =
    override __.ToString() = "ε"

let epsilon = Epsilon()

type Cell<'t> = 
    | NotExecuted
    | Result of 't

[<StructuredFormatDisplay("{u},{a},{v}")>]
type Triple<'u,'a,'v> =
    {
        u:'u
        a:Cell<'a>
        v:'v
    }

type Resumable<'h,'t> = 'h -> 'h * Cell<'t>

type ResumableBuilder() =
    member b.Zero() = 
        fun _ -> epsilon, NotExecuted

    member b.Return(x:'t) =
        fun _ -> epsilon, (Result x)

    member b.Delay(generator:unit->Resumable<'u,'a>) =
        fun x -> generator() x

    member b.ReturnFrom(x) =
        x

    member b.Bind<'u,'a,'v,'b> 
                (
                    f:Resumable<'u,'a>,
                    g:'a->Resumable<'v, 'b>
                 )
                :  Resumable<Triple<'u, 'a, 'v>, 'b>
        = fun (X : Triple<'u, 'a, 'v>) ->
            match X.a with
            | NotExecuted ->
                // The result of f is misssing, we thus
                // advance f's computation by one step
                let u_stepped, a_stepped = f X.u
                { X with u = u_stepped; a = a_stepped }, NotExecuted
    
            | Result _a ->
                /// f's computation has finished. Advance g's computation by one step
                let v_stepped, b_stepped = g _a X.v
                { X with v = v_stepped }, b_stepped


let resumable = new ResumableBuilder()

//////////

open Microsoft.FSharp.Reflection
 
let rec getZeroUntyped<'X> (_type:System.Type) =
    if _type = typeof<Epsilon> then
        box epsilon
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof<Cell<_>> then
        FSharpValue.MakeUnion (FSharpType.GetUnionCases(_type).[0], [||])
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof<Triple<_,_,_>> then
        let type_u = _type.GenericTypeArguments.[0]
        let type_a = _type.GenericTypeArguments.[1]
        let type_v = _type.GenericTypeArguments.[2]
        let zero_of_a = 
            let type_cell_of_a = typedefof<Cell<_>>.MakeGenericType(type_a)
            FSharpValue.MakeUnion(FSharpType.GetUnionCases(type_cell_of_a).[0], [||])
        FSharpValue.MakeRecord (_type, [| getZeroUntyped type_u ;  zero_of_a; getZeroUntyped type_v|])
    else
        invalidArg "_type" "The provided type is not of valid form. The zero element can only be calculated for trace types (those are types used to encode the state of a resumable computation.)"

let getZeroTyped<'X> =
    typeof<'X>
    |> getZeroUntyped
    |> unbox<'X>

(**
Examples:

getZeroTyped<Epsilon>
getZeroTyped<Triple<Epsilon,int,Epsilon>> 
getZeroTyped<Triple<Triple<Epsilon,string,Epsilon>,int,Epsilon>>
getZeroTyped<Triple<Triple<Epsilon,string,Epsilon>,int,Epsilon>>
getZeroTyped<Triple<Triple<Epsilon,Cell<int>,Epsilon>,Cell<int>,Epsilon>>
getZeroTyped<Triple<Triple<Epsilon,Cell<int>,Epsilon>,Cell<int>,Epsilon>>
*)

/////////

let _askString label = 
    printfn "[%s] enter new string: " label
    let x3 = System.Console.ReadLine()
    x3

let _randomNumber label =
    let x2 = System.Random().Next()
    printfn "[%s] new number generated: %d" label x2
    x2


let m =
    resumable {
        // NOTE: do! is not supported yet sow we use let! _ = instead
        let! _ = resumable { printfn "hello2"
                             return () }
        let! x = resumable { //let! z = resumable {return 6}
                             return _askString "a" }
        let w = x + "test"
        let! y = resumable { return _randomNumber "b" }
        let! z = resumable { return _randomNumber "c" }
        return x, y, z, w
    }

let execute (r:Resumable<'h,'b>) a = r a
let r<'a,'v> (r:'v) = { Triple.u = epsilon; a = (NotExecuted:Cell<'a>); v = r} 
let c<'a> () = (NotExecuted:Cell<'a>)

//let s2,t2 = execute m (r << r << r << r <| epsilon)

let s2,t2 = execute m (getZeroTyped<_>)
let s3,_ = execute m s2
let s4,_ = execute m s3
let s5,_ = execute m s4
let s6,rr = execute m s5



