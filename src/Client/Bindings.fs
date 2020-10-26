namespace Feliz.Markdown

open Feliz
open Fable.Core
open Fable.Core.JsInterop

type ITextProperties =
    abstract children: string

type IParagraphProperties =
    abstract children: ReactElement []

type ILinkProperties =
    abstract href: string
    abstract children: ReactElement []

type IHeadingProperties =
    abstract level: int
    abstract children: ReactElement []

type ITableProperties =
    abstract children : ReactElement []

type ITableHeadProperties =
    abstract children : ReactElement []

type ITableBodyProperties =
    abstract children : ReactElement []

type ITableRowProperties =
    abstract children : ReactElement []

type ITableCellProperties =
    abstract isHeader : bool
    abstract children : ReactElement []

type IListProperties =
    abstract children : ReactElement []

type IListItemProperties =
    abstract children : ReactElement []

type IPluginsProperties =
    abstract children : ReactElement []


module markdown =

    type renderers =

        static member inline text (render: ITextProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "text" render)

        static member inline paragraph (render: IParagraphProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "paragraph" render)

        static member inline link (render: ILinkProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "link" render)

        static member inline heading (render: IHeadingProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "heading" render)

        static member inline table (render: ITableProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "table" render)

        static member inline tableHead (render: ITableHeadProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "tableHead" render)

        static member inline tableBody (render: ITableBodyProperties -> ReactElement) =
            unbox<IMarkdownRenderer> (Interop.mkAttr "tableBody" render)

        static member inline tableRow (render : ITableRowProperties -> ReactElement)=
            unbox<IMarkdownRenderer> (Interop.mkAttr "tableRow" render)

        static member inline tableCell (render : ITableCellProperties -> ReactElement)=
            unbox<IMarkdownRenderer> (Interop.mkAttr "tableCell" render)

        static member inline list (render : IListProperties -> ReactElement)=
            unbox<IMarkdownRenderer> (Interop.mkAttr "list" render)

        static member inline listItem (render : IListProperties -> ReactElement)=
            unbox<IMarkdownRenderer> (Interop.mkAttr "listItem" render)



type IMarkdownPlugin = interface end


type markdown =

    static member inline plugins (plugins : IMarkdownPlugin list) =
        Interop.mkAttr "plugins" (plugins |> List.toArray)


type ITableOfContents = interface end

module TableOfContents =
    [<ImportDefault("remark-toc")>]
    let tocObj : ITableOfContents = jsNative


type TableOfContents =

    static member inline toc () =
        Browser.Dom.console.log("toc", TableOfContents.tocObj)
        unbox<IMarkdownPlugin> (TableOfContents.tocObj)

namespace Feliz.Recharts

open Feliz
open Fable.Core
open Fable.Core.JsInterop

module legend =

    [<Erase>]
    type verticalAlign =
        static member inline top = Interop.mkAttr "verticalAlign" "top"
        static member inline bottom = Interop.mkAttr "verticalAlign" "bottom"
        static member inline left = Interop.mkAttr "verticalAlign" "left"

namespace Feliz.Recharts

open Feliz
open Fable.Core
open Fable.Core.JsInterop

[<Erase>]
type scatterChart =
    static member inline width (value: int) = Interop.mkAttr "width" value
    /// The height of chart container.
    static member inline height (value: int) = Interop.mkAttr "height" value
    static member inline children (elements: ReactElement list) = prop.children elements
    /// The sizes of whitespace around the container.
    ///
    /// Default value `{ top: 5, right: 5, bottom: 5, left: 5 }`
    static member inline margin(?top: int, ?right: int, ?left: int, ?bottom: int) =
        let margin = createObj [
            "top" ==> Option.defaultValue 0 top
            "right" ==> Option.defaultValue 0 right
            "left" ==> Option.defaultValue 0 left
            "bottom" ==> Option.defaultValue 0 bottom
        ]

        Interop.mkAttr "margin" margin

    static member inline onClick (handler: ChartMouseEvent<'label, 'payload> -> unit) =
        Interop.mkAttr "onClick" <|
            fun eventArgs ->
                if isNullOrUndefined eventArgs || Interop.objectHas [ "isTooltipActive" ] eventArgs
                then ignore()
                else handler eventArgs

    static member inline onMouseEnter (handler: ChartMouseEvent<'label, 'payload> -> unit) =
        Interop.mkAttr "onMouseEnter" <|
            fun eventArgs ->
                if isNullOrUndefined eventArgs || Interop.objectHas [ "isTooltipActive" ] eventArgs
                then ignore()
                else handler eventArgs

    static member inline onMouseMove (handler: ChartMouseEvent<'label, 'payload> -> unit) =
        Interop.mkAttr "onMouseMove" <|
            fun eventArgs ->
                if isNullOrUndefined eventArgs || Interop.objectHas [ "isTooltipActive" ] eventArgs
                then ignore()
                else handler eventArgs

    static member inline onMouseLeave (handler: unit -> unit) = Interop.mkAttr "onMouseLeave" handler
    static member inline onMouseUp (handler: ChartMouseEvent<'label, 'payload> -> unit) =
        Interop.mkAttr "onMouseUp" <|
            fun eventArgs ->
                if isNullOrUndefined eventArgs || Interop.objectHas [ "isTooltipActive" ] eventArgs
                then ignore()
                else handler eventArgs

    static member inline onMouseDown (handler: ChartMouseEvent<'label, 'payload> -> unit) =
        Interop.mkAttr "onMouseDown" <|
            fun eventArgs ->
                if isNullOrUndefined eventArgs || Interop.objectHas [ "isTooltipActive" ] eventArgs
                then ignore()
                else handler eventArgs

namespace Feliz.Recharts

open System
open Feliz
open Fable.Core

[<Erase>]
type scatter =
    static member inline name (value : string) = Interop.mkAttr "name" value
    static member inline xAxisId (value: string) = Interop.mkAttr "xAxisId" value
    static member inline yAxisId (value: string) = Interop.mkAttr "yAxisId" value
    static member inline xAxisId (value: int) = Interop.mkAttr "xAxisId" value
    static member inline yAxisId (value: int) = Interop.mkAttr "yAxisId" value
    ///// The source data, in which each element is an object.
    //static member inline points (points: seq<'a>) = Interop.mkAttr "points" (Seq.toArray points)
    ///// The source data, in which each element is an object.
    //static member inline points (points: 'a list) = Interop.mkAttr "points" (List.toArray points)
    ///// The source data, in which each element is an object.
    //static member inline porints (points: 'a array) = Interop.mkAttr "points" points
    ///// If set false, animation of area will be disabled.
    static member inline isAnimationActive (value: bool) = Interop.mkAttr "isAnimationActive" value
    /// Specifies when the animation should begin, the unit of this option is ms.
    static member inline animationBegin (value: int) = Interop.mkAttr "animationBegin" value
    /// Specifies the duration of animation, the unit of this option is ms. Default is `1500ms`.
    static member inline animationDuration (value: int) = Interop.mkAttr "animationDuration" value
    /// Specifies the duration of animation. Default is `1500ms`.
    static member inline animationDuration (value: TimeSpan) = Interop.mkAttr "animationDuration" value.TotalMilliseconds
    /// The source data, in which each element is an object.
    static member inline data (values: seq<'a>) = Interop.mkAttr "data" (Seq.toArray values)
    /// The source data, in which each element is an object.
    static member inline data (values: 'a list) = Interop.mkAttr "data" (List.toArray values)
    /// The source data, in which each element is an object.
    static member inline data (values: 'a array) = Interop.mkAttr "data" values
    static member inline fill (color: string) = Interop.mkAttr "fill" color

[<Erase>]
module scatter =
    [<Erase>]
    type legendType =
        static member inline line = Interop.mkAttr "legendType" "line"
        static member inline square = Interop.mkAttr "legendType" "square"
        static member inline rect = Interop.mkAttr "legendType" "rect"
        static member inline circle = Interop.mkAttr "legendType" "circle"
        static member inline cross = Interop.mkAttr "legendType" "cross"
        static member inline diamond = Interop.mkAttr "legendType" "diamond"
        static member inline star = Interop.mkAttr "legendType" "star"
        static member inline triangle = Interop.mkAttr "legendType" "triangle"
        static member inline wye = Interop.mkAttr "legendType" "wye"
        static member inline none = Interop.mkAttr "legendType" "none"

    /// The type of easing function. Default is `ease`.
    [<Erase>]
    type animationEasing =
        static member inline ease = Interop.mkAttr "animationEasing" "ease"
        static member inline easeIn = Interop.mkAttr "animationEasing" "ease-in"
        static member inline easeOut = Interop.mkAttr "animationEasing" "ease-out"
        static member inline easeInOut = Interop.mkAttr "animationEasing" "ease-in-out"
        static member inline linear = Interop.mkAttr "animationEasing" "linear"


namespace Feliz.Recharts

open Feliz
open Fable.Core
open Fable.Core.JsInterop

type ITooltipProperty = interface end

[<Erase>]
type tooltip =
    static member inline separator (value: string) = Interop.mkAttr "separator" value
    static member inline offset (value: int) = Interop.mkAttr "offset" value
    static member inline filterNull (value: bool) = Interop.mkAttr "filterNull" value
    static member inline cursor (value: bool) = Interop.mkAttr "cursor" value
    static member inline cursor(?stroke: string, ?strokeWidth: int) =
        let cursor = createObj [
            "stroke" ==> Option.defaultValue "" stroke
            "strokeWidth" ==> Option.defaultValue 0 strokeWidth
        ]

        Interop.mkAttr "cursor" cursor
    static member inline active (value: bool) = Interop.mkAttr "active" value
    static member inline content (render: ITooltipProperty list -> ReactElement) = Interop.mkAttr "content" render
