/// How to get default value of 'a * 'b.

/// Problem: defaultof<'a*'*b> returns null instead of  defaultof<'a*'> , defaultof<'a*'*b>

// For instance this is null
Unchecked.defaultof<unit*unit>

// whereas default of unit is ()
Unchecked.defaultof<unit>

/// And even with a custom type
type AA<'a,'b> =
| XX of 'a * 'b
/// This also returns null!!!
Unchecked.defaultof<AA<unit,unit>>



type Pair<'a,'b> () =
     class
     end

type X =
    member x.pairDefault<'t when 't : not struct>() = null

type XX<'a,'b> =
    member x.pairDefault< ^a,^b,^t when ^t :> Pair< ^a, ^b>> () =
        (x.pairDefault<'a>(), x.pairDefault<'b>())

let x = 3,5

type XXX =
    member __.pairDefault<'t> () =
        if typeof<'t>.GetGenericTypeDefinition() = typedefof<_*_> then
            (__.pairDefault<atype.GenericTypeArguments.[0]> ()), (__pairDefault<atype.GenericTypeArguments.[1]>())
        else
            

let rec myDefaultof (atype:System.Type) =
    let  
    if atype.GetGenericTypeDefinition() = typedefof<_*_> then
        (myDefaultof atype.GenericTypeArguments.[0]), (myDefaultof atype.GenericTypeArguments.[1])
    else
        x.GetType().get

let y = x.GetType()

x.GetType().GenericParameterAttributes


