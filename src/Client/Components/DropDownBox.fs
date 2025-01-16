namespace Components

module DropDownBox =

    open System
    open Fable.Core
    open Fable.Core.JsInterop

    open Components.Types


    [<JSX.Component>]
    let View (props: DropDownItems) =

        let menuItems =
            props.Items
            |> Array.mapi (fun i (s: string) ->
                let s = if s |> String.IsNullOrEmpty then "Geen" else s
                let k = $"{i}.{s}"

                let s = s :> obj // temp fix for: https://github.com/fable-compiler/Fable/issues/3999

                JSX.jsx
                    $"""
                <MenuItem key={k} value={i}>
                    <Typography color="primary" variant="body1" > 
                        {s}
                    </Typography>
                </MenuItem>
                """)

        let onChange (e: obj) =
            props.Items[e?target?value] |> props.Dispatch

        let value =
            match props.Items |> Array.tryFindIndex ((=) props.Value) with
            // when first is none display nothing
            | Some i when i = 0 && props.FirstIsNone -> ""
            // otherwise display the value
            | Some i -> string i
            // display nothing of no value can be found
            | None -> ""

        let sxFc = {| padding = 1; minWidth = 200 |}
        let sxIl = {| padding = 1 |}

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
