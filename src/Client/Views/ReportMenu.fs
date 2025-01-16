namespace Views


module ReportMenu =

    open Elmish
    open Feliz
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React

    open Shared
    open Components.Types


    module private Elmish =


        let drawerWidth = 250

        type State =
            {|
                filter: Filter
                item: string
                showDiagnoses: bool
            |}

        type Msg =
            | ShowDiagnoses
            | FilterChanged of Filter


        let init filter item =
            {|
                filter = filter
                item = item
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
            | FilterChanged filter ->
                printfn "filter changed to: %A" filter
                let state = {| state with filter = filter |}
                state, Cmd.ofEffect (fun _ -> state |> dispatch)
            | ShowDiagnoses ->
                let state = {| state with showDiagnoses = true |}
                state, Cmd.ofEffect (fun _ -> state |> dispatch)


    open Elmish


    let private renderTreeItem id label children dispatch =
        let props =
            {|
                id = id
                label = label
                children = children
                dispatch = dispatch
            |}

        JSX.jsx
            $"""
        import React from 'react';
    
        <React.Fragment key={id} >
            {Components.TreeItem.View props}            
        </React.Fragment>
        """


    let private renderTree items =
        let props = {| items = items |}

        Components.SimpleTreeView.View(props)


    [<JSX.Component>]
    let View
        (props:
            {|
                data: TreeData[]
                isOpen: bool
                toggle: unit -> unit
                filter: Filter
                currentItem: string
                dispatch: State -> unit
            |})
        =

        let state, dispatch =
            React.useElmish (init props.filter props.currentItem, update props.dispatch, [||])

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

            Components.DropDownBox.View
                {
                    Items = Filter.mapping |> Array.map snd
                    Value = value
                    Dispatch = dispatch
                    Label = "Filter"
                    FirstIsNone = true
                }

        let dispatchItem id =
            fun _ ->
                if id <> props.currentItem then
                    props.dispatch ({| state with item = id |})

        let rec treeItems data =
            let map d =
                let children =
                    if d.children |> Array.isEmpty then
                        [||]
                    else
                        d.children |> treeItems

                renderTreeItem d.id d.label children (dispatchItem d.id)

            data |> Array.map (fun d -> d.id, d) |> Array.mapKeyEls map

        let tree = props.data |> treeItems |> renderTree

        let bxSx = {| width = drawerWidth; padding = 1 |}

        let dbSx = {| padding = 2 |}

        JSX.jsx
            $"""
            import Drawer from '@mui/material/Drawer';
            import Box from '@mui/material/Box';
            import Divider from '@mui/material/Divider';
            import Typography from '@mui/material/Typography';

            <Drawer
                open={props.isOpen}
                onClose={props.toggle}
                anchor="left"
                variant="persistent"
                >
                <Box sx={bxSx} role="presentation" >
                    {showDiagnoses}
                    <Divider />
                    <Box sx={dbSx} >
                        {dropDown}
                    </Box>
                    <Divider />
                    {tree}
                </Box>
            </Drawer>
            """
