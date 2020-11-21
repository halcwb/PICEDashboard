namespace Components

module TreeViewDrawer =

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI

    open Informedica.PICE.Shared.Types

    let drawerWidth = 300

    type State = Filter

    type Msg = 
        | FilterChanged of Filter

    type Data =
        {
            id : string
            label : string
            children : Data list
        }

    let init () = NoFilter,Cmd.none

    let update msg _ =
        match msg with
        | FilterChanged f -> 
            printfn "filter changed to: %A" f
            f, Cmd.none

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
                style.marginTop 20
                style.padding 10
                style.flexGrow 1
                style.width drawerWidth
            ]
        |}
    )

    let private comp =
        React.functionComponent("treeview", fun (props : {| data: Data list; isOpen : bool; dispatch : (Filter * string) -> unit |}) ->
            let classes = useStyles ()
            let state, dispatch = React.useElmish(init, update, [||])
            
            let dropdown =
                let dispatch s =
                    match s with
                    |_ when s = "Neonaat" -> Neonate |> AgeFilter
                    |_ -> NoFilter
                    |> FilterChanged
                    |> dispatch
                [
                    "Neonaat"
                ]
                |> DropDownBox.render "" true "Filter" dispatch

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
                        treeItem.onLabelClick (fun _ -> props.dispatch (state, d.id))
                        treeItem.children (d.children |> create)
                    ]
                )

            let treeView = 
                Mui.treeView [
                    
                    treeView.defaultExpandIcon (Icons.expandMoreIcon "")
                    treeView.defaultCollapseIcon (Icons.chevronRightIcon "")
//                    prop.style [ style.padding 10 ]
                    prop.children (props.data |> create)
                ]

            Html.div [
                prop.className classes.root
                prop.children [
                    Mui.drawer [
                        drawer.open' props.isOpen
                        drawer.variant.persistent
                        drawer.anchor.left
//                        prop.className classes.drawer
                        drawer.classes.paper classes.drawer
                        drawer.children [
                            // this makes sure that the content of the drawer is
                            // below the app bar
                            Html.div [ prop.className classes.toolbar ]
                            dropdown
                            Html.div [ 
                                prop.style [ style.marginTop 20 ]
                            ]
                            treeView
                        ]
                    ]

                ]

            ]
        )

    let render data isOpen dispatch = comp ({| data = data; isOpen = isOpen; dispatch = dispatch |})