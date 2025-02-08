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


    let private renderTree dispatch data =
        let props = {| data = data; dispatch = dispatch |}

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
            fun () ->
                if id <> props.currentItem then
                    props.dispatch ({| state with item = id |})


        let tree = renderTree dispatchItem props.data

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
