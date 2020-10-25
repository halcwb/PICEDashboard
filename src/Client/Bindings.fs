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
