namespace Components

module DropDownBox =

    open System
    open Fable.Core
    open Fable.Core.JsInterop

    open Shared


    [<JSX.Component>]
    let View (props: DropDownItems) =

        let menuItems =
            props.Items
            |> List.mapi (fun i (s: string) ->
                let s = if s |> String.IsNullOrEmpty then "Geen" else s

                JSX.jsx
                    $"""
                <MenuItem key={i} value={i}>
                    <Typography color="primary" variant="body1" > 
                        {s}
                    </Typography>
                </MenuItem>
                """)

        let onChange (e: obj) =
            Logging.log $"DropDownBox: {props.Label} changed to " e
            props.Items[e?target?value] |> props.Dispatch

        let value =
            match props.Items |> List.tryFindIndex ((=) props.Value) with
            // when first is none display nothing
            | Some i when i = 0 && props.FirstIsNone -> ""
            // otherwise display the value
            | Some i -> string i
            // display nothing of no value can be found
            | None -> ""

        let sxFc = {| padding = 1; minWidth = 200 |}
        let sxIl = {| padding = 0 |}
        let sxSl = {| padding = 1 |}

        JSX.jsx
            $"""
            import FormControl from '@mui/material/FormControl';
            import InputLabel from '@mui/material/InputLabel';
            import MenuItem from '@mui/material/MenuItem';
            import Select from '@mui/material/Select';
            import Typography from '@mui/material/Typography';

            <FormControl sx={sxFc} >
                <InputLabel sx = {sxIl}>
                    {props.Label}
                </InputLabel>
                <Select
                    value={value}
                    onChange={onChange}
                >
                    {menuItems}
                </Select>
            </FormControl>
        """
