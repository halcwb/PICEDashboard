namespace Pages


module Statistics =

    open System

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop
    open Feliz.Markdown
    
    open Informedica.PICE.Shared

    open Utils

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 2)
                style.paddingTop (theme.spacing 1)
                style.color (theme.palette.primary.dark)
            ]

            form = styles.create [
            ]

            div = styles.create [
                style.marginBottom (theme.spacing 1)
            ]

            head = styles.create [
                style.backgroundColor theme.palette.primary.dark
                style.color theme.palette.common.white
            ]

        |}
    )


    let private comp =
        React.functionComponent("statistics", fun (props : {| stats : Types.Statistics |}) ->
            // let model, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            
            Markdown.markdown [
                markdown.source props.stats.Html
                markdown.escapeHtml false
                markdown.renderers [
                    markdown.renderers.heading (fun props ->
                        Mui.typography [
                        match props.level with
                        | 1 -> typography.variant.h3
                        | 2 -> typography.variant.h4
                        | 3 -> typography.variant.h5
                        | 4 -> typography.variant.h6
                        | 5 -> typography.variant.body1
                        | 6 -> typography.variant.body2
                        | _ -> ()
//                        typography.paragraph true
                        typography.color.primary
                        prop.style [ style.marginTop 50 ]
                        typography.children props.children
                        ]
                    )

                    markdown.renderers.table (fun props ->
//                        Browser.Dom.console.log ("table: ", box props)
                        Mui.tableContainer [
                            Mui.table props.children
                        ]
                    )

                    markdown.renderers.tableHead (fun props ->
//                        Browser.Dom.console.log ("head: ", box props)
                        Mui.tableHead props.children
                    )

                    markdown.renderers.tableBody (fun props ->
//                        Browser.Dom.console.log ("body: ", box props)
                        Mui.tableBody props.children
                    )

                    markdown.renderers.tableRow (fun props ->
//                        Browser.Dom.console.log ("row: ", box props)
                        Mui.tableRow [
//                            prop.className classes.row
                            tableRow.hover true
                            tableRow.children  props.children
                        ]
                    )

                    markdown.renderers.tableCell (fun props ->
//                        Browser.Dom.console.log ("cell: ", box props)
                        Mui.tableCell [
                            if props.isHeader then prop.className classes.head
                            tableCell.children props.children
                        ]
                    )

                    markdown.renderers.list (fun props ->
                        Mui.list [
                            list.children props.children
                        ]
                    )

                    markdown.renderers.listItem (fun props ->
                        Mui.listItem [
                            listItem.divider true
                            listItem.button true
                            let children = 
                                Mui.typography [
                                    typography.variant.body2
                                    typography.children props.children
                                ]
                            listItem.children children
                        ]
                    )

                    markdown.renderers.paragraph (fun props ->
                        Browser.Dom.console.log ("paragraph: ", box props)
                        Mui.container [
                            prop.style [ style.marginBottom 10 ]
                            container.children props.children
                        ]
                    )
                ]
            ]        
        )


    let render stats = comp ({| stats = stats |})