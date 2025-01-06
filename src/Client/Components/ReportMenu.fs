namespace Components


module ReportMenu =

    open Elmish
    open Feliz
    open Fable.Core
    open Fable.React

    open Shared


    module private Elmish =


        let drawerWidth = 300

        type State =
            {|
                filter: Filter
                item: string
                showDiagnoses: bool
            |}

        type Msg =
            | ShowDiagnoses
            | FilterChanged of Filter


        let init filter =
            {|
                filter = filter
                item = "0"
                showDiagnoses = false
            |},
            Cmd.none


        let update
            dispatch
            msg
            (state:
                {|
                    filter: Filter
                    item: string
                    showDiagnoses: bool
                |})
            =
            match msg with
            | FilterChanged f ->
                printfn "filter changed to: %A" f
                let state = {| state with filter = f |}
                state, Cmd.ofEffect (fun _ -> state |> dispatch)
            | ShowDiagnoses ->
                let state = {| state with showDiagnoses = true |}
                state, Cmd.ofEffect (fun _ -> state |> dispatch)


    open Elmish


    [<JSX.Component>]
    let View
        (props:
            {|
                data: TreeData list
                isOpen: bool
                filter: Filter
                dispatch: State -> unit
            |})
        =

        let state, dispatch =
            React.useElmish (init props.filter, update props.dispatch, [||])

        let showDiagnoses =
            JSX.jsx
                $"""
            import Button from '@mui/material/Button';
            import Typography from '@mui/material/Typography';

            <Button onClick={fun _ -> ShowDiagnoses |> dispatch} >
                <Typography color="textPrimary">ga naar diagnoses</Typography>
            </Button>
            """

        let dropDown =
            let value =
                state.filter
                |> Filter.filterToString
                |> function
                    | Some(_, s) -> s
                    | None -> ""

            let dispatch s =
                match s |> Filter.stringToFilter with
                | Some(f, _) -> f
                | _ -> NoFilter
                |> FilterChanged
                |> dispatch

            DropDownBox.View
                {
                    Items = Filter.mapping |> List.map snd
                    Value = value
                    Dispatch = dispatch
                    Label = "Filter"
                    FirstIsNone = true
                }

        let handleKeyDown id =
            fun _ -> props.dispatch ({| state with item = id |})

        let rec treeItems data =
            data
            |> List.map (fun d ->
                JSX.jsx
                    $""" 
                    <TreeItem key={d.id} itemId={d.id} label= {d.label} onKeyDown = {handleKeyDown} >
                        {d.children |> treeItems}
                    </TreeItem>
                    """)

        let bxSx = {| width = drawerWidth; padding = 1 |}
        let dbSx = {| padding = 2 |}

        JSX.jsx
            $"""
            import Drawer from '@mui/material/Drawer';
            import Box from '@mui/material/Box';
            import Divider from '@mui/material/Divider';
            import Typography from '@mui/material/Typography';
            import {{ SimpleTreeView }} from '@mui/x-tree-view/SimpleTreeView';
            import {{ TreeItem }} from '@mui/x-tree-view/TreeItem';

            <Drawer
                open={props.isOpen}
                anchor="left"
                variant="persistent">
                <Box sx={bxSx} role="presentation" >
                    {showDiagnoses}
                    <Divider />
                    <Box sx={dbSx} >
                        {dropDown}
                    </Box>
                    <Divider />
                    <SimpleTreeView>
                        {props.data |> treeItems}
                    </SimpleTreeView>
                </Box>
            </Drawer>
            """


(*
        let showDiagnoses =
            Mui.button
                [
                    prop.onClick (fun _ -> ShowDiagnoses |> dispatch)
                    prop.children
                        [
                            Mui.typography [ typography.color.textPrimary; prop.text "ga naar diagnoses" ]
                        ]
                ]

        let dropdown =
            let value =
                state.filter
                |> Filter.filterToString
                |> function
                    | Some(_, s) -> s
                    | None -> ""


            Filter.mapping
            |> List.map snd
            |> DropDownBox.render value true "Filter" dispatch

        let rec create data : ReactElement list =
            data
            |> List.map (fun d ->
                Mui.treeItem
                    [
                        treeItem.nodeId d.id
                        treeItem.label
                            [
                                Mui.typography
                                    [ prop.className classes.label; typography.variant.button; prop.text d.label ]
                            ]
                        treeItem.onLabelClick (fun _ -> props.dispatch ({| state with item = d.id |}))
                        treeItem.children (d.children |> create)
                    ])

        let treeView =
            Mui.treeView
                [

                    treeView.defaultExpandIcon (Icons.expandMoreIcon "")
                    treeView.defaultCollapseIcon (Icons.chevronRightIcon "")
                    //                    prop.style [ style.padding 10 ]
                    prop.children (props.data |> create)
                ]

        Html.div
            [
                prop.className classes.root
                prop.children
                    [
                        Mui.drawer
                            [
                                drawer.open' props.isOpen
                                drawer.variant.persistent
                                drawer.anchor.left
                                //                        prop.className classes.drawer
                                drawer.classes.paper classes.drawer
                                drawer.children
                                    [
                                        // this makes sure that the content of the drawer is
                                        // below the app bar
                                        Html.div [ prop.className classes.toolbar ]
                                        showDiagnoses
                                        dropdown
                                        Html.div [ prop.style [ style.marginTop 20 ] ]
                                        treeView
                                    ]
                            ]

                    ]

            ]
        *)
