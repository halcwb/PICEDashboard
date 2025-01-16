namespace Views

module DiagnosesMenu =

    open Elmish
    open Feliz
    open Fable.Core
    open Fable.React

    open Shared


    module private Elmish =

        let drawerWidth = 250


        type Msg = | ShowReport


        let init () =
            {|
                showReport = false
                selected = [||]
            |},
            Cmd.none


        let update
            dispatch
            msg
            (state:
                {|
                    showReport: bool
                    selected: string[]
                |})
            =
            match msg with
            | ShowReport ->
                let state = {| state with showReport = true |}
                state, Cmd.ofEffect (fun _ -> state |> dispatch)


    open Elmish


    [<JSX.Component>]
    let View
        (props:
            {|
                isOpen: bool
                toggle: unit -> unit
                diagnoses: (string * int)[]
                selected: string[]
                dispatch:
                    {|
                        showReport: bool
                        selected: string[]
                    |}
                        -> unit
            |})
        =
        let state, dispatch = React.useElmish (init, update props.dispatch, [||])

        let goToReportButton =
            let onClick = fun _ -> ShowReport |> dispatch

            JSX.jsx
                $"""
            import React from 'react';
            import Button from '@mui/material/Button';
            import Typography from '@mui/material/Typography';

            <Button onClick={onClick}>
                <Typography color="textPrimary">ga naar rapport</Typography>
            </Button>
            """

        let diagnosesList =
            props.diagnoses
            |> Array.map (fun (k, v) -> k, sprintf "%s (%A)" k v)
            |> Array.sort
            |> Array.map (fun (value, label) ->
                let chkd = props.selected |> Array.exists ((=) value)

                let onChange =
                    fun b ->
                        Logging.log "onchange" b

                        if b then
                            {|
                                showReport = false
                                selected = props.selected |> Array.append [| value |]
                            |}
                        else
                            {|
                                showReport = false
                                selected = props.selected |> Array.filter ((<>) value)
                            |}
                        |> props.dispatch

                let chkbox =
                    JSX.jsx
                        $"""
                    import React from 'react';
                    import Checkbox from '@mui/material/Checkbox';

                    <Checkbox
                        checked={chkd}
                        onChange={onChange}
                    />
                    """

                JSX.jsx
                    $"""
                import React from 'react';
                import FormControlLabel from '@mui/material/FormControlLabel';
                import Checkbox from '@mui/material/Checkbox';
                import ListItem from '@mui/material/ListItem';
                import List from '@mui/material/List';

                <ListItem>
                    <FormControlLabel
                        control={chkbox}
                        label={label}
                    />
                </ListItem>
                """)

        let sx =
            {|
                maxWidth = drawerWidth
            //                flexShrink = 0
            |}

        JSX.jsx
            $""" 
        import React from 'react';
        import Drawer from '@mui/material/Drawer';
        import Box from '@mui/material/Box';

        <Drawer
            open={props.isOpen}
            onClose={props.toggle}
            anchor="left"
            variant="persistent"
            sx={sx}
        >
            {goToReportButton}
            <List sx={sx}>
                {diagnosesList}
            </List>
        </Drawer>

        """
