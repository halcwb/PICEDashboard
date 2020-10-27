namespace Components


module Markdown =

    open Feliz
    open Feliz.MaterialUI
    open Feliz.Markdown

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            section = styles.create [
                style.backgroundColor theme.palette.primary.main
                style.color theme.palette.common.white
            ]

        |}
    )

    let private comp =
        React.functionComponent("markdown", fun (props : {| text : string |}) ->
            let classes = useStyles()

            Markdown.markdown [
                markdown.source props.text
                markdown.escapeHtml true
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

                            typography.color.primary
                            if props.level = 2 then 
                                prop.className classes.section
                            prop.style [ 
                                style.marginTop 20 
                            ]
                            typography.children props.children
                        ]
                    )

                    markdown.renderers.table (fun props ->
                        Mui.tableContainer [
                            Mui.table props.children
                        ]
                    )

                    markdown.renderers.tableHead (fun props ->
                        Mui.tableHead props.children
                    )

                    markdown.renderers.tableBody (fun props ->
                        Mui.tableBody props.children
                    )

                    markdown.renderers.tableRow (fun props ->
                        Mui.tableRow [
                            tableRow.hover true
                            tableRow.children  props.children
                        ]
                    )

                    markdown.renderers.tableCell (fun props ->
                        Mui.tableCell [
                            if props.isHeader then prop.className classes.section
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
                                    typography.variant.body1
                                    prop.style [ style.fontWeight.bold ]
                                    typography.color.textSecondary
                                    typography.children props.children
                                ]
                            listItem.children children
                        ]
                    )

                    markdown.renderers.paragraph (fun props ->
                        Mui.container [
                            prop.style [ style.marginTop 10; style.paddingLeft 0 ]
                            let children =
                                Mui.typography [
                                    typography.color.textSecondary
                                    typography.children props.children
                                ]
                            container.children children
                        ]
                    )
                ]
            ]


        )

    let render text = comp({| text = text |})
