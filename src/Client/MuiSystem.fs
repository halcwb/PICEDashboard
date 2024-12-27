module rec Glutinum

open Fable.Core
open Fable.Core.JsInterop
open System

[<AbstractClass>]
[<Erase>]
type Exports =
    [<Import("experimental_sx", "REPLACE_ME_WITH_MODULE_NAME")>]
    static member experimental_sx() : obj = nativeOnly


/// <summary>
/// The <c>css</c> function accepts arrays as values for mobile-first responsive styles.
/// Note that this extends to non-theme values also. For example <c>display=['none', 'block']</c>
/// will also works.
/// </summary>
type ResponsiveStyleValue<'T> = U3<'T, ResizeArray<'T option>, ResponsiveStyleValue.U3.Case3>


module ResponsiveStyleValue =

    module U3 =

        [<AllowNullLiteral>]
        [<Interface>]
        type Case3 =
            [<EmitIndexer>]
            abstract member Item: key: string -> 'T option with get, set
