namespace Components

module TreeViewDrawer =

    open Feliz
    open Feliz.MaterialUI
    open Fable.MaterialUI

    let drawerWidth = 300

    type Data =
        {
            id : string
            label : string
            children : Data list
        }

    let createData id label children =
        {
            id = id
            label = label
            children = children
        }

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            root = styles.create [
                style.display.flex
            ]
            label = styles.create [
                style.color theme.palette.primary.dark
            ]
            toolbar = styles.create [
                yield! theme.mixins.toolbar
            ]
            drawer = styles.create [
                style.flexGrow 1
                style.width drawerWidth
            ]
        |}
    )

    let private comp =
        React.functionComponent("treeview", fun (props : {| data: Data list; dispatch : string -> unit |}) ->
            let classes = useStyles ()

            let rec create data : ReactElement list =
                data
                |> List.map (fun d ->
                    Mui.treeItem [ 
                        treeItem.nodeId d.id
                        treeItem.label [
                            Mui.typography [
                                prop.className classes.label
                                typography.variant.button
                                prop.text d.label 
                            ]
                        ]
                        treeItem.onLabelClick (fun _ -> d.id |> props.dispatch)
                        treeItem.children (d.children |> create)
                    ]
                )

            let treeView = 
                Mui.treeView [
                    treeView.defaultExpandIcon (Icons.expandMoreIcon "")
                    treeView.defaultCollapseIcon (Icons.chevronRightIcon "")
                    prop.style [ style.padding 20 ]
                    prop.children (props.data |> create)
                ]

            Html.div [
                prop.className classes.root
                prop.children [
                    Mui.drawer [
                        drawer.open' true
                        drawer.variant.persistent
                        drawer.anchor.left
//                        prop.className classes.drawer
                        drawer.classes.paper classes.drawer
                        drawer.children [
                            // this makes sure that the content of the drawer is
                            // below the app bar
                            Html.div [ prop.className classes.toolbar ]
                            treeView
                        ]
                    ]

                ]

            ]
        )

    let render data dispatch = comp ({| data = data; dispatch = dispatch |})