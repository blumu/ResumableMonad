module Zero

open Microsoft.FSharp.Reflection

let rec getZeroUntyped<'X> (_type:System.Type) =
    if _type = typeof<unit> then
        box ()
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof<Option<_>> then
        FSharpValue.MakeUnion (FSharpType.GetUnionCases(_type).[0], [||])
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof< _*_> then
        FSharpValue.MakeTuple (_type.GenericTypeArguments |> Array.map getZeroUntyped, _type)
    elif _type.IsGenericType && _type.GetGenericTypeDefinition() = typedefof< _*_*_ > then
        FSharpValue.MakeTuple (_type.GenericTypeArguments |> Array.map getZeroUntyped, _type)
    else
        invalidArg "_type" (sprintf @"The provided type is not of valid form. The zero element can only be calculated for trace types (i.e. types used to encode the state of a resumable computation): %A" _type)

let getZeroTyped<'X> =
    typeof<'X>
    |> getZeroUntyped
    |> unbox<'X>
